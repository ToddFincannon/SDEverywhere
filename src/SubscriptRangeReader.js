import path from 'path';
import { ModelParser } from 'antlr4-vensim'
import R from 'ramda'
import XLSX from 'xlsx'
import ModelReader from './ModelReader.js'
import { Subscript } from './Subscript.js'
import { cFunctionName, matchRegex, readCsv } from './Helpers.js'

export default class SubscriptRangeReader extends ModelReader {
  constructor(modelDirname) {
    super()
    // The model directory is required when reading data files for GET DIRECT SUBSCRIPT.
    this.modelDirname = modelDirname
    // Index names from a subscript list or GET DIRECT SUBSCRIPT
    this.indNames = []
    // Dimension mappings with model names
    this.modelMappings = []
  }
  visitModel(ctx) {
    let subscriptRanges = ctx.subscriptRange()
    if (subscriptRanges) {
      for (let subscriptRange of subscriptRanges) {
        subscriptRange.accept(this)
      }
    }
  }
  visitSubscriptRange(ctx) {
    // When entering a new subscript range definition, reset the properties that will be filled in.
    this.indNames = []
    this.modelMappings = []
    // All subscript ranges have a dimension name.
    let modelName = ctx.Id().getText()
    // Visit children to fill in the subscript range definition.
    super.visitSubscriptRange(ctx)
    // Create a new subscript range definition from Vensim-format names.
    // The family is provisionally set to the dimension name.
    // It will be updated to the maximal dimension if this is a subdimension.
    // The mapping value contains dimensions and indices in the toDim.
    // It will be expanded and inverted to fromDim indices later.
    Subscript(modelName, this.indNames, modelName, this.modelMappings)
  }
  visitSubscriptList(ctx) {
    // A subscript list can appear in either a subscript range or mapping.
    let subscripts = R.map(id => id.getText(), ctx.Id())
    if (ctx.parentCtx.ruleIndex === ModelParser.RULE_subscriptRange) {
      this.indNames = subscripts
    }
    if (ctx.parentCtx.ruleIndex === ModelParser.RULE_subscriptMapping) {
      this.mappingValue = subscripts
    }
  }
  visitSubscriptMapping(ctx) {
    let toDim = ctx.Id().getText()
    // If a subscript list is part of the mapping, mappingValue will be set by visitSubscriptList.
    this.mappingValue = []
    super.visitSubscriptMapping(ctx)
    this.modelMappings.push({ toDim, value: this.mappingValue })
  }
  visitSubscriptSequence(ctx) {
    // Construct index names from the sequence start and end indices.
    // This assumes the indices begin with the same string and end with numbers.
    let r = /^(.*?)(\d+)$/
    let ids = R.map(id => id.getText(), ctx.Id())
    let matches = R.map(id => r.exec(id), ids)
    if (matches[0][1] === matches[1][1]) {
      let prefix = matches[0][1]
      let start = parseInt(matches[0][2])
      let end = parseInt(matches[1][2])
      for (let i = start; i <= end; i++) {
        this.indNames.push(prefix + i)
      }
    }
  }
  visitCall(ctx) {
    // A subscript range can have a GET DIRECT SUBSCRIPT call on the RHS.
    let fn = cFunctionName(ctx.Id().getText())
    if (fn === '_GET_DIRECT_SUBSCRIPT') {
      super.visitCall(ctx)
    }
  }
  visitExprList(ctx) {
    // We assume the only call that ends up here is GET DIRECT SUBSCRIPT.
    let args = R.map(
      arg => matchRegex(arg, /'(.*)'/),
      R.map(expr => expr.getText(), ctx.expr())
    )
    let pathname = args[0]
    let delimiter = args[1]
    let firstCell = args[2]
    let lastCell = args[3]
    // let prefix = args[4]
    // If lastCell is a column letter, scan the column, else scan the row.
    let dataAddress = XLSX.utils.decode_cell(firstCell)
    let col = dataAddress.c
    let row = dataAddress.r
    let nextCell
    if (isNaN(parseInt(lastCell))) {
      nextCell = () => row++
    } else {
      nextCell = () => col++
    }
    // Read subscript names from the CSV file at the given position.
    let csvPathname = path.resolve(this.modelDirname, pathname)
    let data = readCsv(csvPathname, delimiter)
    let indexName = data[row][col]
    while (indexName != null) {
      this.indNames.push(indexName)
      nextCell()
      indexName = data[row] != null ? data[row][col] : null
    }
    super.visitExprList(ctx)
  }
}
