import antlr4 from 'antlr4'
import { ModelLexer, ModelParser } from 'antlr4-vensim'
import R from 'ramda'
import B from 'bufx'
import yaml from 'js-yaml'
import toposort from './toposort.js'
import VariableReader from './VariableReader.js'
import VarNameReader from './VarNameReader.js'
import SubscriptRangeReader from './SubscriptRangeReader.js'
import EquationReader from './EquationReader.js'
import Variable from './Variable.js'
import {
  addIndex,
  allAliases,
  allDimensions,
  indexNamesForSubscript,
  isDimension,
  isIndex,
  normalizeSubscripts,
  sub,
  subscriptFamilies
} from './Subscript.js'
import { decanonicalize, isIterable, listConcat, strlist, vlog, vsort } from './Helpers.js'

let variables = []

// Also keep variables in a map (with `varName` as key) for faster lookup
const variablesByName = new Map()

let nonAtoANames = Object.create(null)
// Set true for diagnostic printing of init, aux, and level vars in sorted order.
const PRINT_SORTED_VARS = false
// Set true to print dependency graphs before they are sorted.
const PRINT_INIT_GRAPH = false
const PRINT_AUX_GRAPH = false
const PRINT_LEVEL_GRAPH = false

function read(parseTree, spec, extData, directData, modelDirname) {
  // Some arrays need to be separated into variables with individual indices to
  // prevent eval cycles. They are manually added to the spec file.
  let specialSeparationDims = spec.specialSeparationDims
  // Subscript ranges must be defined before reading variables that use them.
  readSubscriptRanges(parseTree, spec.dimensionFamilies, spec.indexFamilies, modelDirname)
  // Read variables from the model parse tree.
  readVariables(parseTree, specialSeparationDims, directData)
  // Analyze model equations to fill in more details about variables.
  analyze()
  // Check that all input and output vars in the spec actually exist in the model.
  checkSpecVars(spec, extData)
  // Remove variables that are not referenced by an input or output variable.
  removeUnusedVariables(spec)
}
function readSubscriptRanges(tree, dimensionFamilies, indexFamilies, modelDirname) {
  // Read subscript ranges from the model.
  let subscriptRangeReader = new SubscriptRangeReader(modelDirname)
  subscriptRangeReader.visitModel(tree)
  let allDims = allDimensions()
  // Expand dimensions that appeared in subscript range definitions into indices.
  // Repeat until there are only indices in dimension values.
  let dimFoundInValue
  do {
    dimFoundInValue = false
    for (let dim of allDims) {
      if (dim.value !== '') {
        let value = R.flatten(R.map(subscript => (isDimension(subscript) ? sub(subscript).value : subscript), dim.value))
        if (!R.equals(value, dim.value)) {
          dimFoundInValue = true
          dim.value = value
          dim.size = value.length
        }
      }
    }
  } while (dimFoundInValue)

  // Fill in subscript aliases from their model families.
  for (let dim of allAliases()) {
    if (dim.value === '') {
      let refDim = sub(dim.family)
      dim.value = refDim.value
      dim.size = refDim.size
      dim.modelValue = refDim.modelValue
      allDims.push(dim)
    }
  }

  // Update the families of dimensions. At this point, all dimensions have their family
  // provisionally set to their own dimension name.
  let dimComparator = (dim1, dim2) => {
    // Sort dimensions by size ascending, by name descending.
    if (dim1.size < dim2.size) {
      return -1
    } else if (dim1.size > dim2.size) {
      return 1
    } else if (dim1.name > dim2.name) {
      return -1
    } else if (dim1.name < dim2.name) {
      return 1
    } else {
      return 0
    }
  }
  for (let dim of allDims) {
    // Try looking up the family in the spec file dimension families if they exist.
    if (dimensionFamilies && dimensionFamilies[dim.name]) {
      dim.family = dimensionFamilies[dim.name]
    } else {
      // Find the dimension in this family with the largest number of values.
      // This is the "maximal" dimension that serves as the subscript family.
      // If two dimensions have the same maximal size, choose the one that comes
      // first in alpha sort order, by convention.
      // Take the first index in the dimension.
      let index = dim.value[0]
      let familyDims = R.sort(
        dimComparator,
        R.filter(thisDim => R.contains(index, thisDim.value), allDims)
      )
      if (familyDims.length > 0) {
        dim.family = R.last(familyDims).name
      } else {
        console.error(`No family found for dimension ${dim.name}`)
      }
    }
  }

  // Define indices in order from the maximal (family) dimension.
  // Until now, only dimensions have been defined. We wait until dimension families have been
  // determined to define indices, so that they will belong to exactly one dimension (the family).
  for (let dim of allDims) {
    if (dim.family === dim.name) {
      for (let i = 0; i < dim.value.length; i++) {
        addIndex(dim.value[i], i, dim.family)
      }
    }
  }

  // When there is a subscript mapping, the mapping value pulled from the subscript range
  // in the model is either a map-to dimension with the same cardinality as the map-from
  // dimension, or a list of subscripts in the map-to dimension with the same cardinality
  // as the map-from dimension. The mapping value must be transformed into a list of
  // map-from indices in one-to-one correspondence with the map-to indices.
  for (let fromDim of allDims) {
    for (let toDimName in fromDim.mappings) {
      let toDim = sub(toDimName)
      let mappingValue = fromDim.mappings[toDimName]
      let invertedMappingValue = []
      if (R.isEmpty(mappingValue)) {
        // When there is no list of map-to subscripts, list fromDim indices.
        invertedMappingValue = fromDim.value
      } else {
        // The mapping value is a list of map-to subscripts.
        // List fromDim indices in the order in which they map onto toDim indices.
        // Indices are filled in the mapping value by map-to index number as they
        // occur in the map-from dimension.
        let setMappingValue = (toIndNumber, fromIndName) => {
          if (Number.isInteger(toIndNumber) && toIndNumber >= 0 && toIndNumber < toDim.size) {
            invertedMappingValue[toIndNumber] = fromIndName
          } else {
            console.error(
              `ERROR: map-to index "${toSubName}" not found when mapping from dimension "${fromDim.name}" index "${fromIndName}"`
            )
          }
        }
        for (let i = 0; i < fromDim.value.length; i++) {
          let fromIndName = fromDim.value[i]
          let toSubName = mappingValue[i]
          let toSub = sub(toSubName)
          if (isDimension(toSubName)) {
            // Fill in indices from a dimension in the mapping value.
            for (let toIndName of toSub.value) {
              let toIndNumber = toDim.value.indexOf(toIndName)
              setMappingValue(toIndNumber, fromIndName)
            }
          } else {
            // Fill in a single index from an index in the mapping value.
            let toIndNumber = toDim.value.indexOf(toSub.name)
            setMappingValue(toIndNumber, fromIndName)
          }
        }
      }
      // Replace toDim subscripts in the mapping value with fromDim subscripts that map to them.
      fromDim.mappings[toDimName] = invertedMappingValue
    }
  }
}
function readVariables(tree, specialSeparationDims, directData) {
  // Read all variables in the model parse tree.
  // This populates the variables table with basic information for each variable
  // such as the var name and subscripts.
  let variableReader = new VariableReader(specialSeparationDims, directData)
  variableReader.visitModel(tree)
  // Add a placeholder variable for the exogenous variable Time.
  let v = new Variable(null)
  v.modelLHS = 'Time'
  v.varName = '_time'
  addVariable(v)
}
function analyze() {
  // Analyze the RHS of each equation in stages after all the variables are read.
  // Find non-apply-to-all vars that are defined with more than one equation.
  findNonAtoAVars()
  // Set the refId for each variable. Only non-apply-to-all vars include subscripts in the refId.
  setRefIds()
  // Read the RHS to list the refIds of vars that are referenced and set the var type.
  readEquations()
}

function checkSpecVars(spec, extData) {
  // Look up each var in the spec and issue and error message if it does not exist.

  function check(varNames, specType) {
    if (isIterable(varNames)) {
      for (let varName of varNames) {
        if (!R.contains('[', varName)) {
          if (!varWithRefId(varName)) {
            // Look for a variable in external data.
            if (extData.has(varName)) {
              // console.error(`found ${specType} ${varName} in extData`)
              // Copy data from an external file to an equation that does a lookup.
              let lookup = R.reduce(
                (a, p) => listConcat(a, `(${p[0]}, ${p[1]})`, true),
                '',
                Array.from(extData.get(varName))
              )
              let modelEquation = `${decanonicalize(varName)} = WITH LOOKUP(Time, (${lookup}))`
              addEquation(modelEquation)
            } else {
              console.error(`${specType} variable ${varName} not found in the model or external data sources`)
            }
          }
        }
      }
    }
  }

  if (spec) {
    // If the spec file contains `input/outputVarNames` (with full Vensim variable names)
    // convert those to C names first.  Otherwise, use `input/outputNames` which are already
    // assumed to be valid C names.
    if (spec.inputVarNames) {
      spec.inputVars = R.map(cName, spec.inputVarNames)
    }
    if (spec.outputVarNames) {
      spec.outputVars = R.map(cName, spec.outputVarNames)
    }

    check(spec.inputVars, 'input')
    check(spec.outputVars, 'output')
  }
}

function removeUnusedVariables(spec) {
  // Remove any variables that are not referenced by an input or output variable.
  // This ensures that only computations that are relevant to the outputs are performed.

  // Only remove dead code if we have an explicit set of inputs and outputs
  if (!spec.outputVars || spec.outputVars.length === 0 || !spec.inputVars || spec.inputVars.length === 0) {
    return
  }

  // Keep track of all variable names that are referenced somewhere.  Note that we
  // don't attempt to track specific "ref ids" (e.g. `_some_variable[_subscript]`)
  // but instead just track generic variable names (e.g. `_some_variable`).  This
  // ensures that we include all subscripts for a variable, which might mean we
  // include some subscripts that aren't needed, but it is safer than trying to
  // eliminate those and possibly omit something that is needed.
  const referencedVarNames = []

  // Add the given variable name to the list of referenced variables, if it's not
  // already there.
  const recordUsedVarName = varName => {
    if (!referencedVarNames.includes(varName)) {
      referencedVarNames.push(varName)
    }
  }

  // Add the given variable to the list of referenced variables, and do the same for
  // some special things (i.e., lookups) that it might reference.
  const recordUsedVariable = v => {
    // Add the variable to the list of referenced variables
    recordUsedVarName(v.varName)

    // Include any lookup variables that are referenced by this variable
    if (v.referencedLookupVarNames) {
      for (const lookupVarName of v.referencedLookupVarNames) {
        recordUsedVarName(lookupVarName)
      }
    }

    // Look through the list of function names that are referenced by this
    // variable and see if any of them are lookups (which should be included in
    // our list of referenced variables)
    if (v.referencedFunctionNames) {
      for (const fnName of v.referencedFunctionNames) {
        // Convert the function name (e.g. `__damage_lookup`) to a lookup var name (chop off
        // the leading underscore)
        const lookupName = fnName.slice(1)
        const varForFn = varWithName(lookupName)
        if (varForFn && varForFn.isLookup()) {
          recordUsedVarName(varForFn.varName)
        }
      }
    }
  }

  // Walk the reference tree rooted at the given var and record it (and anything
  // that it references) as being "used".
  const referencedRefIds = []
  const recordRefsOfVariable = v => {
    // If this variable is subscripted, we need to record all subscript variants;
    // `refIdsWithName` will return those.  We also need to record all variables
    // that are referenced by this variable, either directly (`v.references`) or
    // in an "INITIAL" expression (`v.initReferences`).  It's OK if we end up with
    // duplicates in this list, because we will examine each reference only once.
    let refIds = refIdsWithName(v.varName)
    refIds = refIds.concat(v.references)
    refIds = refIds.concat(v.initReferences)
    for (const refId of refIds) {
      if (!referencedRefIds.includes(refId)) {
        referencedRefIds.push(refId)
        const refVar = varWithRefId(refId)
        recordUsedVariable(refVar)
        recordRefsOfVariable(refVar)
      }
    }
  }

  // Always keep special vars used by SDE
  recordUsedVarName('_initial_time')
  recordUsedVarName('_final_time')
  recordUsedVarName('_saveper')
  recordUsedVarName('_time_step')

  // Keep all input variables
  for (const inputVarName of spec.inputVars) {
    for (const v of varsWithName(inputVarName)) {
      recordUsedVariable(v)
    }
  }

  // Keep all output variables and the variables they depend on
  for (const outputVarName of spec.outputVars) {
    // The outputVars can include a raw index, e.g. `_output_var[0]`,
    // which isn't an actual "ref id", so we'll just derive the
    // var name by chopping off the index part.
    const outputVarBaseName = outputVarName.split('[')[0]
    for (const v of varsWithName(outputVarBaseName)) {
      recordUsedVariable(v)
      recordRefsOfVariable(v)
    }
  }

  // Filter out unneeded variables so we're left with the minimal set of variables to emit
  variables = R.filter(v => referencedVarNames.includes(v.varName), variables)

  // Rebuild the variables-by-name map
  variablesByName.clear()
  for (const v of variables) {
    let varsForName = variablesByName.get(v.varName)
    if (!varsForName) {
      varsForName = []
      variablesByName.set(v.varName, varsForName)
    }
    varsForName.push(v)
  }
}

//
// Analysis helpers
//
function findNonAtoAVars() {
  // Find variables with multiple instances with the same var name, which makes them
  // elements in a non-apply-to-all array. This function constructs the nonAtoANames list.
  let names = varNames()
  function areSubsEqual(vars, i) {
    // Scan the subscripts for each var at position i in normal order.
    // Return true if the subscript is the same for all vars with that name.
    let subscript = vars[0].subscripts[i]
    for (let v of vars) {
      if (v.subscripts[i] !== subscript) {
        return false
      }
    }
    return true
  }
  R.forEach(name => {
    let vars = varsWithName(name)
    if (vars.length > 1) {
      // This is a non-apply-to-all array. Construct the exansion dims array for it.
      // The expansion dim is true at each dim position where the subscript varies.
      let numDims = vars[0].subscripts.length
      let expansionDims = []
      for (let i = 0; i < numDims; i++) {
        expansionDims[i] = !areSubsEqual(vars, i)
      }
      nonAtoANames[name] = expansionDims
    }
  }, varNames())
}
function addNonAtoAVar(varName, expansionDims) {
  nonAtoANames[varName] = expansionDims
}
function setRefIds() {
  // Set the refId for each var. This requires knowing which vars are non-apply-to-all.
  R.forEach(v => {
    v.refId = refIdForVar(v)
  }, variables)
}
function readEquations() {
  // Augment variables with information from their equations.
  // This requires a refId for each var so that actual refIds can be resolved for the reference list.
  R.forEach(v => {
    let equationReader = new EquationReader(v)
    equationReader.read()
  }, variables)
}
function addEquation(modelEquation) {
  // Add an equation in Vensim model format.
  let chars = new antlr4.InputStream(modelEquation)
  let lexer = new ModelLexer(chars)
  let tokens = new antlr4.CommonTokenStream(lexer)
  let parser = new ModelParser(tokens)
  parser.buildParseTrees = true
  let tree = parser.equation()
  // Read the var and add it to the Model var table.
  let variableReader = new VariableReader()
  variableReader.visitEquation(tree)
  let v = variableReader.var
  // Fill in the refId.
  v.refId = refIdForVar(v)
  // Finish the variable by parsing the RHS.
  let equationReader = new EquationReader(v)
  equationReader.read()
}
//
// Model API
//
function addVariable(v) {
  // Add the variable to the variables list.
  variables.push(v)

  // Add to the map of variables by name
  let varsForName = variablesByName.get(v.varName)
  if (!varsForName) {
    varsForName = []
    variablesByName.set(v.varName, varsForName)
  }
  varsForName.push(v)
}
function isNonAtoAName(varName) {
  return R.has(varName, nonAtoANames)
}
function expansionFlags(varName) {
  return nonAtoANames[varName]
}
function allVars() {
  // Return all vars except placeholders.
  function isNotPlaceholderVar(v) {
    return v.varName !== '_time'
  }
  return R.filter(isNotPlaceholderVar, variables)
}
function constVars() {
  return vsort(varsOfType('const'))
}
function lookupVars() {
  return vsort(varsOfType('lookup'))
}
function dataVars() {
  return vsort(varsOfType('data'))
}
function auxVars() {
  // console.error('AUX VARS');
  return sortVarsOfType('aux')
}
function levelVars() {
  // console.error('LEVEL VARS');
  return sortVarsOfType('level')
}
function initVars() {
  // console.error('INIT VARS');
  return sortInitVars()
}
function varWithRefId(refId) {
  const findVarWithRefId = rid => {
    // First see if we have a map key where ref id matches the var name
    let varsForName = variablesByName.get(rid)
    if (varsForName) {
      const v = R.find(R.propEq('refId', rid), varsForName)
      if (v) {
        return v
      }
    }

    // Failing that, chop off the subscript part of the ref id and
    // find the variables that share that name
    const varNamePart = rid.split('[')[0]
    varsForName = variablesByName.get(varNamePart)
    if (varsForName) {
      const v = R.find(R.propEq('refId', rid), varsForName)
      if (v) {
        return v
      }
    }

    return undefined
  }

  // Find a variable from a reference id.
  // A direct reference will find scalar vars, apply-to-all arrays, and non-apply-to-all array
  // elements defined by individual index.
  let refVar = findVarWithRefId(refId)
  if (!refVar) {
    // Look at variables with the reference's varName to find one with matching subscripts.
    let refIdParts = splitRefId(refId)
    let refVarName = refIdParts.varName
    let refSubscripts = refIdParts.subscripts
    let varRefIds = refIdsWithName(refVarName)
    for (const varRefId of varRefIds) {
      let { subscripts } = splitRefId(varRefId)
      // Compare subscripts at each position in normal order. If the var name does not have subscripts,
      // the match will succeed, since the var is an apply-to-all array that includes the refId.
      let matches = true
      for (let pos = 0; pos < subscripts.length; pos++) {
        // If both subscripts are an index or dimension, they must match.
        if (
          (isIndex(subscripts[pos]) && isIndex(refSubscripts[pos])) ||
          (isDimension(subscripts[pos]) && isDimension(refSubscripts[pos]))
        ) {
          if (subscripts[pos] !== refSubscripts[pos]) {
            matches = false
            break
          }
        } else if (isDimension(subscripts[pos]) && isIndex(refSubscripts[pos])) {
          // If the ref subscript is an index and the var subscript is a dimension,
          // they match if the dimension includes the index.
          if (!sub(subscripts[pos]).value.includes(refSubscripts[pos])) {
            matches = false
            break
          }
        } else {
          // We should not encounter a case where the ref subscript is a dimension
          // and the var subscript is an index.
          matches = false
          break
        }
      }
      if (matches) {
        refVar = findVarWithRefId(varRefId)
        break
      }
    }
    if (!refVar) {
      vlog('ERROR: no var found for refId', refId)
      debugger
    }
  }
  return refVar
}
function splitRefId(refId) {
  // Split a refId into component parts with a regular expression matching var name and subscripts.
  let re = /\w+|\[/g
  let inSubs = false
  let varName = ''
  let subscripts = []
  let m
  while ((m = re.exec(refId))) {
    if (m[0] === '[') {
      inSubs = true
    } else if (inSubs) {
      subscripts.push(m[0])
    } else {
      varName = m[0]
    }
  }
  // Put subscripts in normal order.
  subscripts = normalizeSubscripts(subscripts)
  return { varName, subscripts }
}
function varWithName(varName) {
  // Find a variable with the given name in canonical form.
  // The function returns the first instance of a non-apply-to-all variable with the name.
  const varsForName = variablesByName.get(varName)
  if (varsForName && varsForName.length > 0) {
    return varsForName[0]
  } else {
    return undefined
  }
}
function varsWithName(varName) {
  // Find all variables with the given name in canonical form.
  return variablesByName.get(varName) || []
}
function refIdsWithName(varName) {
  // Find refIds of all variables with the given name in canonical form.
  return varsWithName(varName).map(v => v.refId)
}
function varNames() {
  // Return a sorted list of var names.
  return R.uniq(Array.from(variablesByName.keys())).sort()
}
function vensimName(cVarName) {
  // Convert a C variable name to a Vensim name.
  let result = cVarName
  // Get the variable name and subscripts with regexes.
  let m = cVarName.match(/(_[A-Za-z0-9_]+)(\[\d+\])?(\[\d+\])?/)
  if (m) {
    let varName = m[1]
    let indexNumbers = []
    if (m[2]) {
      indexNumbers.push(m[2].replace('[', '').replace(']', ''))
      if (m[3]) {
        indexNumbers.push(m[3].replace('[', '').replace(']', ''))
      }
    }
    // Get the subscript families and look up the subscript names.
    let subscripts = []
    let v = varWithName(varName)
    if (v) {
      // Ensure that the C var name is subscripted when the var has subscripts.
      if (R.isEmpty(v.subscripts) || !R.isEmpty(indexNumbers)) {
        m = v.modelLHS.match(/[^\[]+/)
        if (m) {
          result = m[0]
        }
        let families = subscriptFamilies(v.subscripts)
        for (let i = 0; i < families.length; i++) {
          let indexNames = indexNamesForSubscript(families[i])
          let indexNumber = Number.parseInt(indexNumbers[i])
          let indexModelName = decanonicalize(indexNames[indexNumber])
          subscripts.push(indexModelName)
        }
        if (!R.isEmpty(subscripts)) {
          result += `[${subscripts.join(',')}]`
        }
      } else {
        console.error(`${cVarName} has no subscripts in vensimName`)
      }
    } else {
      console.error(`no var with name ${varName} in vensimName`)
    }
  }
  return result
}
function cName(vensimVarName) {
  // Convert a Vensim variable name to a C name.
  // This function requires model analysis to be completed first when the variable has subscripts.
  return new VarNameReader().read(vensimVarName)
}
//
// Helpers for getting lists of vars
//
function varsOfType(varType, vars = null) {
  // Extract vars of the given var type.
  if (!vars) {
    vars = variables
  }
  function pass(v) {
    return v.varType === varType && v.varName !== '_time'
  }
  return R.filter(pass, vars)
}
function sortVarsOfType(varType) {
  if (PRINT_SORTED_VARS) {
    console.error(varType.toUpperCase())
  }

  // Get vars with varType 'aux' or 'level' sorted in dependency order at eval time.
  // Start with vars of the given varType.
  let vars = varsOfType(varType)

  // Accumulate a list of variable dependencies as var pairs.
  let graph = R.unnest(R.map(v => refs(v), vars))
  function refs(v) {
    // Return a list of dependency pairs for all vars referenced by v at eval time.
    let refs = R.map(refId => varWithRefId(refId), v.references)
    // Only consider references having the correct var type.
    // Remove duplicate references.
    refs = R.uniq(R.filter(R.propEq('varType', varType), refs))
    // Return the list of dependencies as refId pairs.
    return R.map(ref => {
      if (v.varType === 'level' && ref.varType === 'level') {
        // Reverse the order of level-to-level references so that level evaluation refers
        // to the value in the previous time step rather than the currently evaluated one.
        return [ref.refId, v.refId]
      } else {
        return [v.refId, ref.refId]
      }
    }, refs)
  }

  // Sort into an lhs dependency list.
  if (PRINT_AUX_GRAPH) printDepsGraph(graph, 'AUX')
  if (PRINT_LEVEL_GRAPH) printDepsGraph(graph, 'LEVEL')
  let deps
  try {
    deps = toposort(graph).reverse()
  } catch (e) {
    console.error(e.message)
    process.exit(1)
  }

  // Turn the dependency-sorted var name list into a var list.
  let sortedVars = varsOfType(
    varType,
    R.map(refId => varWithRefId(refId), deps)
  )

  // Add the ref ids to a set for faster lookup in the next step
  const sortedVarRefIds = new Set()
  for (const v of sortedVars) {
    sortedVarRefIds.add(v.refId)
  }

  // Find vars of the given varType with no dependencies, and add them to the list.
  const nodepVars = R.filter(v => !sortedVarRefIds.has(v.refId), vars)
  const sortedNodepVars = vsort(nodepVars)
  sortedVars = R.concat(sortedNodepVars, sortedVars)

  if (PRINT_SORTED_VARS) {
    sortedVars.forEach((v, i) => console.error(`${v.refId}`))
  }
  return sortedVars
}
function sortInitVars() {
  if (PRINT_SORTED_VARS) {
    console.error('INIT')
  }

  // Get dependencies at init time for vars with init values, such as levels.
  // This will be a subgraph of all dependencies rooted in vars with init values.
  // Therefore, we have to recurse into dependencies starting with those vars.
  let initVars = R.filter(R.propEq('hasInitValue', true), variables)
  // vlog('initVars.length', initVars.length);

  // Copy the list so we can mutate it and have the original list later.
  // This starts a queue of vars to examine. Referenced var will be added to the queue.
  let vars = R.map(v => v.copy(), initVars)
  // printVars(vars);
  // R.forEach(v => { console.error(v.refId); console.error(v.references); }, vars);

  // Keep track of which var ref ids are currently in the queue for faster lookup
  const queueRefIds = new Set()
  for (const v of vars) {
    queueRefIds.add(v.refId)
  }

  // Build a map of dependencies indexed by the lhs of each var.
  const depsMap = new Map()
  while (vars.length > 0) {
    let v = vars.pop()
    queueRefIds.delete(v.refId)
    // console.error(`- ${v.refId} (${vars.length})`);
    addDepsToMap(v)
  }

  function addDepsToMap(v) {
    // Add dependencies of var v to the map when they are not already present.
    // Use init references for vars such as levels that have an initial value.
    let refIds = v.hasInitValue ? v.initReferences : v.references
    // console.error(`${v.refId} ${refIds.length}`);
    if (refIds.length > 0) {
      // console.error(`${v.refId}`);
      // Add dependencies for each referenced var.
      depsMap.set(v.refId, refIds)
      // console.error(`→ ${v.refId}`);
      R.forEach(refId => {
        // Add each dependency onto the queue if it has not already been analyzed.
        if (!depsMap.get(refId)) {
          // console.error(refId);
          let refVar = varWithRefId(refId)
          if (refVar) {
            if (refVar.varType !== 'const' && !queueRefIds.has(refVar.refId)) {
              vars.push(refVar)
              queueRefIds.add(refVar.refId)
              // console.error(`+ ${refVar.refId}`);
            }
          } else {
            console.error(`no var with refId for ${refId}, referenced by ${v.refId}`)
          }
        }
      }, refIds)
    }
  }

  // Construct a dependency graph in the form of [var name, dependency var name] pairs.
  // We use refIds instead of vars here because the deps are stated in refIds.
  let graph = []
  // vlog('depsMap', depsMap);
  for (let refId of depsMap.keys()) {
    R.forEach(dep => graph.push([refId, dep]), depsMap.get(refId))
  }
  if (PRINT_INIT_GRAPH) printDepsGraph(graph, 'INIT')

  // Sort into a reference id dependency list.
  let deps
  try {
    deps = toposort(graph).reverse()
  } catch (e) {
    console.error(e.message)
    process.exit(1)
  }

  // Turn the reference id list into a var list.
  let sortedVars = R.map(refId => varWithRefId(refId), deps)

  // Filter out vars with constant values.
  sortedVars = R.reject(
    R.propSatisfies(varType => varType === 'const' || varType === 'lookup', 'varType'),
    sortedVars
  )

  // Add the ref ids to a set for faster lookup in the next step
  const sortedVarRefIds = new Set()
  for (const v of sortedVars) {
    sortedVarRefIds.add(v.refId)
  }

  // Find vars with init values but no dependencies, and add them to the list.
  const nodepVars = R.filter(v => !sortedVarRefIds.has(v.refId), initVars)
  const sortedNodepVars = vsort(nodepVars)
  sortedVars = R.concat(sortedNodepVars, sortedVars)

  if (PRINT_SORTED_VARS) {
    sortedVars.forEach((v, i) => console.error(`${v.refId}`))
  }
  return sortedVars
}
//
// Helpers for refIds
//
function refIdForVar(v) {
  // Start a reference id using the variable name.
  let refId = v.varName
  // References to apply-to-all arrays reference the entire array, so no subscripts
  // are required in the refId.
  if (v.hasSubscripts() && isNonAtoAName(v.varName)) {
    // Add subscripts already sorted in normal form for references to non-apply-to-all arrays.
    refId += `[${v.subscripts.join(',')}]`
  }
  return refId
}
//
// Helpers for model analysis
//
function printVarList() {
  // Print full information on each var.
  B.clearBuf()
  let vars = R.sortBy(R.prop('refId'), variables)
  for (const v of vars) {
    printVar(v)
  }
  return B.getBuf()
}
function yamlVarList() {
  // Print selected properties of all variable objects to a YAML string.
  let vars = R.sortBy(
    R.prop('refId'),
    R.map(v => filterVar(v), variables)
  )
  return yaml.safeDump(vars)
}
function printVar(v) {
  let nonAtoA = isNonAtoAName(v.varName) ? ' (non-apply-to-all)' : ''
  B.emitLine(`${v.modelLHS}: ${v.varType}${nonAtoA}`)
  if (!v.hasPoints()) {
    B.emitLine(`= ${v.modelFormula}`)
  }
  B.emitLine(`refId(${v.refId})`)
  if (v.hasSubscripts()) {
    B.emitLine(`families(${strlist(subscriptFamilies(v.subscripts))})`)
    B.emitLine(`subscripts(${strlist(v.subscripts)})`)
  }
  if (v.separationDims.length > 0) {
    B.emitLine(`separationDims(${strlist(v.separationDims)})`)
  }
  B.emitLine(`hasInitValue(${v.hasInitValue})`)
  if (v.references.length > 0) {
    B.emitLine(`refs(${strlist(v.references)})`)
  }
  if (v.initReferences.length > 0) {
    B.emitLine(`initRefs(${strlist(v.initReferences)})`)
  }
  // if (v.hasPoints()) {
  //   B.emitLine(R.map(p => `(${p[0]}, ${p[1]})`, v.points));
  // }
  B.emitLine('')
}
function filterVar(v) {
  let varObj = {}
  varObj.refId = v.refId
  varObj.varName = v.varName
  if (v.hasSubscripts()) {
    varObj.subscripts = v.subscripts
    varObj.families = subscriptFamilies(v.subscripts)
  }
  if (v.references.length > 0) {
    varObj.references = v.references
  }
  varObj.hasInitValue = v.hasInitValue
  if (v.initReferences.length > 0) {
    varObj.initReferences = v.initReferences
  }
  varObj.varType = v.varType
  if (v.separationDims.length > 0) {
    varObj.separationDims = v.separationDims
  }
  varObj.modelLHS = v.modelLHS
  varObj.modelFormula = v.modelFormula
  return varObj
}
function printRefIdTest() {
  // Verify that each variable has the correct number of instances of the var name.
  R.forEach(v => {
    let varName = v.varName
    let vars = varsWithName(varName)
    if (v.hasSubscripts()) {
      if (isNonAtoAName(varName)) {
        // A non-apply-to-all array has more than one instance of the var name in practice.
        if (vars.length < 2) {
          vlog('ERROR: only one instance of non-apply-to-all array', varName)
        }
      } else {
        // An apply-to-all array should have only one instance of the var name.
        if (vars.length > 1) {
          vlog('ERROR: more than one instance of apply-to-all array', varName)
          printVars(vars)
        }
      }
    } else {
      // The var is a scalar and should only have one instance of the var name.
      if (vars.length > 1) {
        vlog('ERROR: more than one instance of scalar var', varName)
        printVars(vars)
      }
    }
  }, variables)
  // Verify that each refId in references exists as the refId of a concrete variable.
  R.forEach(v => {
    R.forEach(refId => checkRefVar(refId), v.references)
    R.forEach(refId => checkRefVar(refId), v.initReferences)
  }, variables)
  function checkRefVar(refId) {
    let refVar = R.find(R.propEq('refId', refId), variables)
    if (!refVar) {
      vlog('ERROR: no var for refId', refId)
    }
  }
}
function printRefGraph(varName) {
  // Walk the reference tree rooted at varName and print it out in indented form.
  let printRefs = (v, indent, stack) => {
    for (let refId of v.references) {
      // Exclude a variable here to limit the depth of the search.
      // if (!refId.startsWith('_policy_levels')) {
      if (!stack.includes(refId)) {
        console.log(`${'  '.repeat(indent)}${refId}`)
        let refVar = R.find(R.propEq('refId', refId), variables)
        printRefs(refVar, indent + 1, R.append(refId, stack))
      }
      // }
    }
  }
  for (let v of varsWithName(varName)) {
    console.log(v.varName)
    printRefs(v, 1, [])
  }
}
function printDepsGraph(graph, varType) {
  // The dependency graph is an array of pairs.
  console.error(`${varType} GRAPH`)
  for (const dep of graph) {
    console.error(`${dep[0]} → ${dep[1]}`)
  }
}
export default {
  addEquation,
  addNonAtoAVar,
  addVariable,
  allVars,
  auxVars,
  cName,
  constVars,
  dataVars,
  expansionFlags,
  filterVar,
  initVars,
  isNonAtoAName,
  levelVars,
  lookupVars,
  printRefGraph,
  printRefIdTest,
  printVarList,
  read,
  refIdForVar,
  refIdsWithName,
  variables,
  varNames,
  varsWithName,
  varWithName,
  varWithRefId,
  vensimName,
  yamlVarList
}
