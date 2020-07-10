const R = require('ramda')
const XLSX = require('xlsx')
const { ModelLexer, ModelParser } = require('antlr4-vensim')
const ModelReader = require('./ModelReader')
const ModelLHSReader = require('./ModelLHSReader')
const LoopIndexVars = require('./LoopIndexVars')
const Model = require('./Model')
const {
  dimensionNames,
  extractMarkedDims,
  hasMapping,
  isDimension,
  isIndex,
  normalizeSubscripts,
  separatedVariableIndex,
  sub
} = require('./Subscript')
const {
  canonicalName,
  cdbl,
  cFunctionName,
  isArrayFunction,
  isDelayFunction,
  isSmoothFunction,
  isTrendFunction,
  listConcat,
  newTmpVarName,
  strToConst,
  vlog
} = require('./Helpers')

module.exports = class EquationGen extends ModelReader {
  constructor(variable, extData, directData, initMode = false) {
    super()
    // the variable we are generating code for
    this.var = variable
    // external data map from DAT files
    this.extData = extData
    // direct data workbooks from Excel files
    this.directData = directData
    // initMode is true for vars with separate init-time code generation
    this.initMode = initMode
    // Maps of LHS subscript families to loop index vars for lookup on the RHS
    this.loopIndexVars = new LoopIndexVars(['i', 'j', 'k'])
    this.arrayIndexVars = new LoopIndexVars(['v', 'w'])
    // The LHS for array variables includes subscripts in normal form.
    this.lhs = this.var.varName + this.lhsSubscriptGen(this.var.subscripts)
    // formula expression channel
    this.exprCode = ''
    // comments channel
    this.comments = []
    // temporary variable channel
    this.tmpVarCode = []
    // subscript loop opening channel
    this.subscriptLoopOpeningCode = []
    // subscript loop closing channel
    this.subscriptLoopClosingCode = []
    // array function code buffer
    this.arrayFunctionCode = ''
    // the marked dimensions for an array function
    this.markedDims = []
    // stack of var names inside an expr
    this.varNames = []
    // components extracted from arguments to VECTOR ELM MAP
    this.vemVarName = ''
    this.vemSubscripts = []
    this.vemIndexDim = ''
    this.vemIndexBase = 0
    this.vemOffset = ''
    // components extracted from arguments to VECTOR SORT ORDER
    this.vsoVarName = ''
    this.vsoOrder = ''
    this.vsoTmpName = ''
    this.vsoTmpDimName = ''
    // components extracted from arguments to VECTOR SELECT
    this.vsSelectionArray = ''
    this.vsNullValue = ''
    this.vsAction = 0
    this.vsError = ''
  }
  generate() {
    // Generate code for the variable in either init or eval mode.
    if (this.var.isData()) {
      return this.generateData()
    }
    if (this.var.isLookup()) {
      return this.generateLookup()
    }
    // Show the model var as a comment for reference.
    this.comments.push(`  // ${this.var.modelLHS} = ${this.var.modelFormula.replace('\n', '')}`)
    // Initialize array variables with dimensions in a loop for each dimension.
    let dimNames = dimensionNames(this.var.subscripts)
    // Turn each dimension name into a loop with a loop index variable.
    // If the variable has no subscripts, nothing will be emitted here.
    this.subscriptLoopOpeningCode = R.concat(
      this.subscriptLoopOpeningCode,
      R.map(dimName => {
        let i = this.loopIndexVars.index(dimName)
        return `  for (size_t ${i} = 0; ${i} < ${sub(dimName).size}; ${i}++) {`
      }, dimNames)
    )
    // Walk the parse tree to generate code into all channels.
    // Use this to examine code generation for a particular variable.
    // if (this.var.refId === '') {
    //   debugger
    // }
    this.visitEquation(this.var.eqnCtx)
    // Either emit constant list code or a regular var assignment.
    let formula = `  ${this.lhs} = ${this.exprCode};`
    // Close the assignment loops.
    this.subscriptLoopClosingCode = R.concat(
      this.subscriptLoopClosingCode,
      R.map(dimName => `  }`, dimNames)
    )
    // Assemble code from each channel into final var code output.
    return this.comments.concat(this.subscriptLoopOpeningCode, this.tmpVarCode, formula, this.subscriptLoopClosingCode)
  }
  //
  // Helpers
  //
  currentVarName() {
    let n = this.varNames.length
    return n > 0 ? this.varNames[n - 1] : undefined
  }
  lookupName() {
    // Convert a call name into a lookup name.
    return canonicalName(this.currentFunctionName()).slice(1)
  }
  emit(text) {
    if (isArrayFunction(this.currentFunctionName())) {
      // Emit code to the array function code buffer if we are in an array function.
      this.arrayFunctionCode += text
    } else {
      // Otherwise emit code to the expression code channel.
      this.exprCode += text
    }
  }
  cVarOrConst(expr) {
    // Get either a constant or a var name in C format from a parse tree expression.
    let value = expr.getText().trim()
    if (value === ':NA:') {
      return '_NA_'
    } else {
      let v = Model.varWithName(canonicalName(value))
      if (v) {
        return v.varName
      } else {
        let d = parseFloat(value)
        if (Number.isNaN(d)) {
          d = 0
        }
        return cdbl(d)
      }
    }
  }
  constValue(c) {
    // Get a numeric value from a constant var name in model form.
    // Return 0 if the value is not a numeric string or const variable.
    let value = parseFloat(c)
    if (!Number.isNaN(value)) {
      return value
    }
    // Look up the value as a symbol name and return the const value.
    value = 0
    let v = Model.varWithName(canonicalName(c))
    if (v && v.isConst()) {
      value = parseFloat(v.modelFormula)
      if (Number.isNaN(value)) {
        value = 0
      }
    }
    return value
  }

  lookupDataNameGen(subscripts) {
    // Construct a name for the static data array associated with a lookup variable.
    return R.map(subscript => {
      if (isDimension(subscript)) {
        let i = this.loopIndexVars.index(subscript)
        return `_${subscript}_${i}_`
      } else {
        return `_${sub(subscript).value}_`
      }
    }, subscripts).join('')
  }
  lhsSubscriptGen(subscripts) {
    // Construct C array subscripts from subscript names in the variable's normal order.
    return R.map(subscript => {
      if (isDimension(subscript)) {
        let i = this.loopIndexVars.index(subscript)
        return `[${subscript}[${i}]]`
      } else {
        return `[${sub(subscript).value}]`
      }
    }, subscripts).join('')
  }
  rhsSubscriptGen(subscripts) {
    // Normalize RHS subscripts.
    try {
      subscripts = normalizeSubscripts(subscripts)
    } catch (e) {
      console.error('ERROR: normalizeSubscripts failed in rhsSubscriptGen')
      vlog('this.var.refId', this.var.refId)
      vlog('this.currentVarName()', this.currentVarName())
      vlog('subscripts', subscripts)
      throw e
    }
    // Get the loop index var name source.
    let cSubscripts = R.map(rhsSub => {
      if (isIndex(rhsSub)) {
        // Return the index number for an index subscript.
        return `[${sub(rhsSub).value}]`
      } else {
        // The subscript is a dimension.
        // Get the loop index variable, matching the previously emitted for loop variable.
        let i
        if (this.markedDims.includes(rhsSub)) {
          i = this.arrayIndexVars.index(rhsSub)
        } else {
          // Use the single index name for a separated variable if it exists.
          let separatedIndexName = separatedVariableIndex(rhsSub, this.var, subscripts)
          if (separatedIndexName) {
            return `[${sub(separatedIndexName).value}]`
          }
          // See if we need to apply a mapping because the RHS dim is not found on the LHS.
          try {
            let found = this.var.subscripts.findIndex(lhsSub => sub(lhsSub).family === sub(rhsSub).family)
            if (found < 0) {
              // Find the  mapping from the RHS subscript to a LHS subscript.
              for (let lhsSub of this.var.subscripts) {
                if (hasMapping(rhsSub, lhsSub)) {
                  // console.error(`${this.var.refId} hasMapping ${rhsSub} → ${lhsSub}`);
                  i = this.loopIndexVars.index(lhsSub)
                  return `[__map${rhsSub}${lhsSub}[${i}]]`
                }
              }
            }
          } catch (e) {
            debugger
            throw e
          }
          // There is no mapping, so use the loop index for this dim family on the LHS.
          i = this.loopIndexVars.index(rhsSub)
        }
        // Return the dimension and loop index for a dimension subscript.
        return `[${rhsSub}[${i}]]`
      }
    }, subscripts).join('')
    return cSubscripts
  }
  vemSubscriptGen() {
    // VECTOR ELM MAP replaces one subscript with a calculated vemOffset from a base index.
    let subscripts = normalizeSubscripts(this.vemSubscripts)
    let cSubscripts = R.map(rhsSub => {
      if (isIndex(rhsSub)) {
        // Emit the index vemOffset from VECTOR ELM MAP for the index subscript.
        return `[${this.vemIndexDim}[${this.vemIndexBase} + ${this.vemOffset}]]`
      } else {
        let i = this.loopIndexVars.index(rhsSub)
        return `[${rhsSub}[${i}]]`
      }
    }, subscripts).join('')
    return cSubscripts
  }
  vsoSubscriptGen(subscripts) {
    // _VECTOR_SORT_ORDER will iterate over the last subscript in its first arg.
    let i = this.loopIndexVars.index(subscripts[0])
    if (subscripts.length > 1) {
      this.vsoVarName += `[${subscripts[0]}[${i}]]`
      i = this.loopIndexVars.index(subscripts[1])
      this.vsoTmpDimName = subscripts[1]
    } else {
      this.vsoTmpDimName = subscripts[0]
    }
    // Emit the tmp var subscript just after emitting the tmp var elsewhere.
    this.emit(`[${this.vsoTmpDimName}[${i}]]`)
  }
  functionIsLookup() {
    // See if the function name in the current call is actually a lookup.
    // console.error(`isLookup ${this.lookupName()}`);
    let v = Model.varWithName(this.lookupName())
    return v && v.isLookup()
  }
  generateLookup() {
    // Construct the name of the data array, which is based on the associated lookup var name,
    // with any subscripts tacked on the end.
    const dataName = this.var.varName + '_data_' + this.lookupDataNameGen(this.var.subscripts)
    if (this.initMode) {
      // In init mode, create the `Lookup`, passing in a pointer to the static data array declared earlier.
      // TODO: Make use of the lookup range
      return [`  ${this.lhs} = __new_lookup(${this.var.points.length}, /*copy=*/false, ${dataName});`]
    } else {
      // In decl mode, declare a static data array that will be used to create the associated `Lookup`
      // at init time. Using static arrays is better for code size, helps us avoid creating a copy of
      // the data in memory, and seems to perform much better when compiled to wasm when compared to the
      // previous approach that used varargs + copying, especially on constrained (e.g. iOS) devices.
      let data = R.reduce((a, p) => listConcat(a, `${cdbl(p[0])}, ${cdbl(p[1])}`, true), '', this.var.points)
      return [`double ${dataName}[${this.var.points.length * 2}] = { ${data} };`]
    }
  }
  generateData() {
    let result = []
    if (this.initMode) {
      // If direct data exists for this variable, copy it from the workbook into one or more lookups.
      if (this.var.directDataArgs) {
        let { tag, sheetName, timeRowOrCol, startCell } = this.var.directDataArgs
        let workbook = this.directData.get(tag)
        if (workbook) {
          let sheet = workbook.Sheets[sheetName]
          if (sheet) {
            let indexNum = 0
            if (!R.isEmpty(this.var.subscripts)) {
              // Generate a lookup for a separated index in the variable's dimension.
              // TODO allow the index to be in either position of a 2D subscript
              let ind = sub(this.var.subscripts[0])
              indexNum = ind.value
            }
            result.push(this.generateDirectDataLookup(sheet, timeRowOrCol, startCell, indexNum))
          } else {
            console.error(`direct data worksheet ${sheetName} tagged ${tag} not found`)
          }
        } else {
          console.error(`direct data workbook tagged ${tag} not found`)
        }
      } else {
        // If there is external data for this variable, copy it from an external file to a lookup.
        let data = this.extData.get(this.var.varName)
        if (data) {
          let args = R.reduce(
            (a, p) => listConcat(a, `${cdbl(p[0])}, ${cdbl(p[1])}`, true),
            '',
            Array.from(data.entries())
          )
          result = [`  ${this.lhs} = __new_lookup(${data.size}, /*copy=*/true, (double[]){ ${args} });`]
        } else {
          console.error(`data variable ${this.var.varName} not found in external data sources`)
        }
      }
    }
    return result
  }
  generateDirectDataLookup(sheet, timeRowOrCol, startCell, indexNum) {
    // Read a row or column of data as (time, value) pairs from the worksheet.
    let dataCol, dataRow, dataCell, timeCol, timeRow, timeCell, nextCell
    let lookupData = ''
    let lookupSize = 0
    let cell = (c, r) => sheet[XLSX.utils.encode_cell({ c, r })]
    let dataAddress = XLSX.utils.decode_cell(startCell)
    dataCol = dataAddress.c
    dataRow = dataAddress.r
    if (isNaN(parseInt(timeRowOrCol))) {
      // Time values are in a column.
      timeCol = XLSX.utils.decode_col(timeRowOrCol)
      timeRow = dataRow
      dataCol += indexNum
      nextCell = () => {
        dataRow++
        timeRow++
      }
    } else {
      // Time values are in a row.
      timeCol = dataCol
      timeRow = XLSX.utils.decode_row(timeRowOrCol)
      dataRow += indexNum
      nextCell = () => {
        dataCol++
        timeCol++
      }
    }
    timeCell = cell(timeCol, timeRow)
    dataCell = cell(dataCol, dataRow)
    while (timeCell && dataCell) {
      lookupData = listConcat(lookupData, `${cdbl(timeCell.v)}, ${cdbl(dataCell.v)}`, true)
      lookupSize++
      nextCell()
      dataCell = cell(dataCol, dataRow)
      timeCell = cell(timeCol, timeRow)
    }
    return [`  ${this.lhs} = __new_lookup(${lookupSize}, /*copy=*/true, (double[]){ ${lookupData} });`]
  }
  //
  // Visitor callbacks
  //
  visitCall(ctx) {
    // Convert the function name from Vensim to C format and push it onto the function name stack.
    // This maintains the name of the current function as its arguments are visited.
    this.callStack.push({ fn: cFunctionName(ctx.Id().getText()) })
    let fn = this.currentFunctionName()
    // Do not emit the function calls in init mode, only the init expression.
    // Do emit function calls inside an init expression (with call stack length > 1).
    if (this.var.hasInitValue && this.initMode && this.callStack.length <= 1) {
      super.visitCall(ctx)
      this.callStack.pop()
    } else if (fn === '_ELMCOUNT') {
      // Replace the function with the value of its argument, emitted in visitVar.
      super.visitCall(ctx)
      this.callStack.pop()
    } else if (isArrayFunction(fn)) {
      // Generate a loop that evaluates array functions inline.
      // Collect information and generate the argument expression into the array function code buffer.
      super.visitCall(ctx)
      // Start a temporary variable to hold the result of the array function.
      let condVar
      let initValue = '0.0'
      if (fn === '_VECTOR_SELECT') {
        initValue = this.vsAction === 3 ? '-DBL_MAX' : '0.0'
        condVar = newTmpVarName()
        this.tmpVarCode.push(`  bool ${condVar} = false;`)
      } else if (fn === '_VMIN') {
        initValue = 'DBL_MAX'
      } else if (fn === '_VMAX') {
        initValue = '-DBL_MAX'
      }
      let tmpVar = newTmpVarName()
      this.tmpVarCode.push(`  double ${tmpVar} = ${initValue};`)
      // Emit the array function loop opening into the tmp var channel.
      for (let markedDim of this.markedDims) {
        let n
        try {
          n = sub(markedDim).size
        } catch (e) {
          console.error(`ERROR: marked dimension "${markedDim}" not found in var ${this.var.refId}`)
          throw e
        }
        let i = this.arrayIndexVars.index(markedDim)
        this.tmpVarCode.push(`	for (size_t ${i} = 0; ${i} < ${n}; ${i}++) {`)
      }
      // Emit the body of the array function loop.
      if (fn === '_VECTOR_SELECT') {
        this.tmpVarCode.push(`    if (bool_cond(${this.vsSelectionArray})) {`)
      }
      if (fn === '_SUM' || (fn === '_VECTOR_SELECT' && this.vsAction === 0)) {
        this.tmpVarCode.push(`	  ${tmpVar} += ${this.arrayFunctionCode};`)
      } else if (fn === '_VMIN') {
        this.tmpVarCode.push(`	  ${tmpVar} = fmin(${tmpVar}, ${this.arrayFunctionCode});`)
      } else if (fn === '_VMAX' || (fn === '_VECTOR_SELECT' && this.vsAction === 3)) {
        this.tmpVarCode.push(`	  ${tmpVar} = fmax(${tmpVar}, ${this.arrayFunctionCode});`)
      }
      if (fn === '_VECTOR_SELECT') {
        this.tmpVarCode.push(`    ${condVar} = true;`)
        this.tmpVarCode.push('    }')
      }
      // Close the array function loops.
      for (let i = 0; i < this.markedDims.length; i++) {
        this.tmpVarCode.push(`	}`)
      }
      this.callStack.pop()
      // Reset state variables that were set down in the parse tree.
      this.markedDims = []
      this.arrayFunctionCode = ''
      // Emit the temporary variable into the formula expression in place of the SUM call.
      if (fn === '_VECTOR_SELECT') {
        this.emit(`${condVar} ? ${tmpVar} : ${this.vsNullValue}`)
      } else {
        this.emit(tmpVar)
      }
    } else if (fn === '_VECTOR_ELM_MAP') {
      super.visitCall(ctx)
      this.emit(`${this.vemVarName}${this.vemSubscriptGen()}`)
      this.callStack.pop()
      this.vemVarName = ''
      this.vemSubscripts = []
      this.vemIndexDim = ''
      this.vemIndexBase = 0
      this.vemOffset = ''
    } else if (fn === '_VECTOR_SORT_ORDER') {
      super.visitCall(ctx)
      let dimSize = sub(this.vsoTmpDimName).size
      let vso = `  double* ${this.vsoTmpName} = _VECTOR_SORT_ORDER(${this.vsoVarName}, ${dimSize}, ${this.vsoOrder});`
      // Inject the VSO call into the loop opening code that was aleady emitted into that channel.
      this.subscriptLoopOpeningCode.splice(this.var.subscripts.length - 1, 0, vso)
      this.callStack.pop()
      this.vsoVarName = ''
      this.vsoOrder = ''
      this.vsoTmpName = ''
      this.vsoTmpDimName = ''
    } else if (this.functionIsLookup() || this.var.isData()) {
      // A lookup has function syntax but lookup semantics. Convert the function call into a lookup call.
      this.emit(`_LOOKUP(${this.lookupName()}, `)
      super.visitCall(ctx)
      this.emit(')')
      this.callStack.pop()
    } else if (fn === '_ACTIVE_INITIAL') {
      // Only emit the eval-time initialization without the function call for ACTIVE INITIAL.
      super.visitCall(ctx)
    } else if (isSmoothFunction(fn)) {
      // For smooth functions, replace the entire call with the expansion variable generated earlier.
      let smoothVar = Model.varWithRefId(this.var.smoothVarRefId)
      this.emit(smoothVar.varName)
      this.emit(this.rhsSubscriptGen(smoothVar.subscripts))
    } else if (isTrendFunction(fn)) {
      // For delay  functions, replace the entire call with the expansion variable generated earlier.
      let trendVar = Model.varWithRefId(this.var.trendVarName)
      let rhsSubs = this.rhsSubscriptGen(trendVar.subscripts)
      this.emit(`${this.var.trendVarName}${rhsSubs}`)
    } else if (isDelayFunction(fn)) {
      // For delay  functions, replace the entire call with the expansion variable generated earlier.
      let delayVar = Model.varWithRefId(this.var.delayVarRefId)
      let rhsSubs = this.rhsSubscriptGen(delayVar.subscripts)
      this.emit(`(${delayVar.varName}${rhsSubs} / ${this.var.delayTimeVarName}${rhsSubs})`)
    } else {
      // Generate code for ordinary function calls here.
      this.emit(fn)
      this.emit('(')
      super.visitCall(ctx)
      this.emit(')')
      this.callStack.pop()
    }
  }
  visitExprList(ctx) {
    let exprs = ctx.expr()
    let fn = this.currentFunctionName()
    // Split level functions into init and eval expressions.
    if (fn === '_INTEG' || fn === '_SAMPLE_IF_TRUE' || fn === '_ACTIVE_INITIAL') {
      if (this.initMode) {
        // Get the index of the argument holding the initial value.
        let i = 0
        if (fn === '_INTEG' || fn === '_ACTIVE_INITIAL') {
          i = 1
        } else if (fn === '_SAMPLE_IF_TRUE') {
          i = 2
        }
        this.setArgIndex(i)
        exprs[i].accept(this)
      } else {
        // We are in eval mode, not init mode.
        // For ACTIVE INITIAL, emit the first arg without a function call.
        if (fn === '_ACTIVE_INITIAL') {
          this.setArgIndex(0)
          exprs[0].accept(this)
        } else {
          // Emit the variable LHS as the first arg at eval time, giving the current value for the level.
          this.emit(this.lhs)
          this.emit(', ')
          // Emit the remaining arguments by visiting each expression in the list.
          this.setArgIndex(0)
          exprs[0].accept(this)
          if (fn === '_SAMPLE_IF_TRUE') {
            this.emit(', ')
            this.setArgIndex(1)
            exprs[1].accept(this)
          }
        }
      }
    } else if (fn === '_VECTOR_SELECT') {
      this.setArgIndex(0)
      exprs[0].accept(this)
      this.setArgIndex(1)
      exprs[1].accept(this)
      this.setArgIndex(2)
      this.vsNullValue = this.cVarOrConst(exprs[2])
      // TODO implement other actions besides just sum and max
      this.setArgIndex(3)
      this.vsAction = this.constValue(exprs[3].getText().trim())
      // TODO obey the error handling instruction here
      this.setArgIndex(4)
      this.vsError = this.cVarOrConst(exprs[4])
    } else if (fn === '_VECTOR_ELM_MAP') {
      this.setArgIndex(0)
      exprs[0].accept(this)
      this.setArgIndex(1)
      exprs[1].accept(this)
    } else if (fn === '_VECTOR_SORT_ORDER') {
      this.setArgIndex(0)
      exprs[0].accept(this)
      this.setArgIndex(1)
      this.vsoOrder = this.cVarOrConst(exprs[1])
    } else {
      // Ordinary expression lists are completely emitted with comma delimiters.
      for (let i = 0; i < exprs.length; i++) {
        if (i > 0) this.emit(', ')
        this.setArgIndex(i)
        exprs[i].accept(this)
      }
    }
  }
  visitVar(ctx) {
    // Push the var name on the stack and then emit it.
    let id = ctx.Id().getText()
    let varName = canonicalName(id)
    if (isDimension(varName)) {
      if (this.currentFunctionName() === '_ELMCOUNT') {
        // Emit the size of the dimension in place of the dimension name.
        this.emit(`${sub(varName).size}`)
      } else {
        // A subscript masquerading as a variable takes the value of the loop index var plus one
        // (since Vensim indices are one-based).
        let s = this.rhsSubscriptGen([varName])
        // Remove the brackets around the C subscript expression.
        s = s.slice(1, s.length-1)
        this.emit(`(${s} + 1)`)
      }
    } else {
      this.varNames.push(varName)
      if (this.currentFunctionName() === '_VECTOR_SELECT') {
        let argIndex = this.argIndexForFunctionName('_VECTOR_SELECT')
        if (argIndex === 0) {
          this.vsSelectionArray = this.currentVarName()
        } else if (argIndex === 1) {
          this.emit(this.currentVarName())
        }
        super.visitVar(ctx)
      } else if (this.currentFunctionName() === '_VECTOR_ELM_MAP') {
        if (this.argIndexForFunctionName('_VECTOR_ELM_MAP') === 1) {
          this.vemOffset = `(size_t)${this.currentVarName()}`
        }
        super.visitVar(ctx)
      } else if (this.currentFunctionName() === '_VECTOR_SORT_ORDER') {
        if (this.argIndexForFunctionName('_VECTOR_SORT_ORDER') === 0) {
          this.vsoVarName = this.currentVarName()
          this.vsoTmpName = newTmpVarName()
          this.emit(this.vsoTmpName)
        }
        super.visitVar(ctx)
      } else {
        let v = Model.varWithName(this.currentVarName())
        if (v && v.varType === 'data') {
          this.emit(`_LOOKUP(${this.currentVarName()}`)
          super.visitVar(ctx)
          this.emit(', _time)')
        } else {
          this.emit(this.currentVarName())
          super.visitVar(ctx)
        }
      }
      this.varNames.pop()
    }
  }
  visitLookupArg(ctx) {
    // Substitute the previously generated lookup arg var name into the expression.
    if (this.var.lookupArgVarName) {
      this.emit(this.var.lookupArgVarName)
    }
  }
  visitLookupCall(ctx) {
    // Make a lookup argument into a _LOOKUP function call.
    let id = ctx.Id().getText()
    this.varNames.push(canonicalName(id))
    this.emit(`_LOOKUP(${canonicalName(id)}`)
    // Emit subscripts after the var name, if any.
    super.visitLookupCall(ctx)
    this.emit(', ')
    ctx.expr().accept(this)
    this.emit(')')
    this.varNames.pop()
  }
  visitSubscriptList(ctx) {
    // Emit subscripts for a variable occurring on the RHS.
    if (ctx.parentCtx.ruleIndex === ModelParser.RULE_expr) {
      let subscripts = R.map(id => canonicalName(id.getText()), ctx.Id())
      let mergeMarkedDims = () => {
        // Extract all marked dimensions and update subscripts.
        let dims = extractMarkedDims(subscripts)
        // Merge marked dims that were found into the list for this call.
        this.markedDims = R.uniq(R.concat(this.markedDims, dims))
      }
      let fn = this.currentFunctionName()
      if (fn === '_SUM' || fn === '_VMIN' || fn === '_VMAX') {
        mergeMarkedDims()
        this.emit(this.rhsSubscriptGen(subscripts))
      } else if (fn === '_VECTOR_SELECT') {
        let argIndex = this.argIndexForFunctionName('_VECTOR_SELECT')
        if (argIndex === 0) {
          mergeMarkedDims()
          this.vsSelectionArray += this.rhsSubscriptGen(subscripts)
        } else if (argIndex === 1) {
          mergeMarkedDims()
          this.emit(this.rhsSubscriptGen(subscripts))
        }
      } else if (fn === '_VECTOR_ELM_MAP') {
        if (this.argIndexForFunctionName('_VECTOR_ELM_MAP') === 0) {
          this.vemVarName = this.currentVarName()
          // Gather information from the argument to generate code later.
          // The marked dim is an index in the vector argument.
          this.vemSubscripts = subscripts
          for (let subscript of subscripts) {
            if (isIndex(subscript)) {
              let ind = sub(subscript)
              this.vemIndexDim = ind.family
              this.vemIndexBase = ind.value
              break
            }
          }
        } else {
          // Add subscripts to the offset argument.
          this.vemOffset += this.rhsSubscriptGen(subscripts)
        }
      } else if (fn === '_VECTOR_SORT_ORDER') {
        if (this.argIndexForFunctionName('_VECTOR_SORT_ORDER') === 0) {
          this.vsoSubscriptGen(subscripts)
        }
      } else {
        // Add C subscripts to the variable name that was already emitted.
        this.emit(this.rhsSubscriptGen(subscripts))
      }
    }
  }
  visitConstList(ctx) {
    let emitConstAtPos = i => {
      try {
        this.emit(strToConst(exprs[i].getText()))
      } catch (e) {
        debugger
        throw e
      }
    }
    let exprs = ctx.expr()
    // console.error(`visitConstList ${this.var.refId} ${exprs.length} exprs`)
    if (exprs.length === 1) {
      // Emit a single constant into the expression code.
      emitConstAtPos(0)
    } else {
      // Extract a single value from the const list by its index number.
      // All const lists with > 1 value are separated on dimensions in the LHS.
      // The LHS of a separated variable here will contain only index subscripts.
      let numDims = this.var.separationDims.length
      if (numDims === 1) {
        // Find the index that is in the separation dimension.
        let sepDim = sub(this.var.separationDims[0])
        for (let ind of this.var.subscripts) {
          let i = sepDim.value.indexOf(ind)
          if (i >= 0) {
            // Emit the constant at this position in the constant list.
            emitConstAtPos(i)
            break
          }
        }
      } else if (numDims === 2) {
        // Calculate an index into a flattened array by converting the indices to numeric form and looking them up
        // in a C name array listed in the same Vensim order as the constant array in the model.
        let cVarName
        let modelLHSReader = new ModelLHSReader()
        modelLHSReader.read(this.var.modelLHS)
        let cNames = modelLHSReader.names().map(Model.cName)
        // Visit dims in normal order. Find the ind in the dim. Compose the C array expression with numeric indices.
        for (let dim of this.var.separationDims) {
          let sepDim = sub(dim)
          for (let ind of this.var.subscripts) {
            let i = sepDim.value.indexOf(ind)
            if (i >= 0) {
              let indexNum = sub(ind).value
              if (!cVarName) {
                cVarName = `${this.var.varName}[${indexNum}]`
              } else {
                cVarName += `[${indexNum}]`
              }
              break
            }
          }
        }
        // Find the position of the constant in Vensim order from the expanded LHS var list.
        let constPos = R.indexOf(cVarName, cNames)
        if (constPos >= 0) {
          emitConstAtPos(constPos)
          // console.error(`${this.var.refId} position = ${constPos}`)
        } else {
          console.error(`${this.var.refId} → ${cVarName} not found in C names`)
        }
      }
    }
  }
  //
  // Operators, etc.
  //
  visitNegative(ctx) {
    this.emit('-')
    super.visitNegative(ctx)
  }
  visitNot(ctx) {
    this.emit('!')
    super.visitNot(ctx)
  }
  visitPower(ctx) {
    this.emit('pow(')
    ctx.expr(0).accept(this)
    this.emit(', ')
    ctx.expr(1).accept(this)
    this.emit(')')
  }
  visitMulDiv(ctx) {
    ctx.expr(0).accept(this)
    if (ctx.op.type === ModelLexer.Star) {
      this.emit(' * ')
    } else {
      this.emit(' / ')
    }
    ctx.expr(1).accept(this)
  }
  visitAddSub(ctx) {
    ctx.expr(0).accept(this)
    if (ctx.op.type === ModelLexer.Plus) {
      this.emit(' + ')
    } else {
      this.emit(' - ')
    }
    ctx.expr(1).accept(this)
  }
  visitRelational(ctx) {
    ctx.expr(0).accept(this)
    if (ctx.op.type === ModelLexer.Less) {
      this.emit(' < ')
    } else if (ctx.op.type === ModelLexer.Greater) {
      this.emit(' > ')
    } else if (ctx.op.type === ModelLexer.LessEqual) {
      this.emit(' <= ')
    } else {
      this.emit(' >= ')
    }
    ctx.expr(1).accept(this)
  }
  visitEquality(ctx) {
    ctx.expr(0).accept(this)
    if (ctx.op.type === ModelLexer.Equal) {
      this.emit(' == ')
    } else {
      this.emit(' != ')
    }
    ctx.expr(1).accept(this)
  }
  visitAnd(ctx) {
    ctx.expr(0).accept(this)
    this.emit(' && ')
    ctx.expr(1).accept(this)
  }
  visitOr(ctx) {
    ctx.expr(0).accept(this)
    this.emit(' || ')
    ctx.expr(1).accept(this)
  }
  visitKeyword(ctx) {
    var keyword = ctx.Keyword().getText()
    if (keyword === ':NA:') {
      keyword = '_NA_'
    } else if (keyword === ':INTERPOLATE:') {
      keyword = ''
    }
    this.emit(keyword)
  }
  visitConst(ctx) {
    let c = ctx.Const().getText()
    this.emit(strToConst(c))
  }
  visitParens(ctx) {
    this.emit('(')
    super.visitParens(ctx)
    this.emit(')')
  }
}
