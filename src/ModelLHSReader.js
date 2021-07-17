import antlr4 from 'antlr4'
import R from 'ramda'
import { ModelLexer, ModelParser } from 'antlr4-vensim'
import ModelReader from './ModelReader.js'
import { sub, isDimension } from './Subscript.js'
import { canonicalName } from './Helpers.js'

//
// ModelLHSReader parses the LHS of a var in Vensim format and
// constructs a list of var names with indices for subscripted vars.
//
export default class ModelLHSReader extends ModelReader {
  constructor() {
    super()
    this.varName = ''
    this.modelLHSList = []
    this.modelSubscripts = []
  }
  read(modelLHS) {
    // Parse a model LHS and return the var name without subscripts.
    // The names function may be called on this object to retrieve expanded subscript names.
    let chars = new antlr4.InputStream(modelLHS)
    let lexer = new ModelLexer(chars)
    let tokens = new antlr4.CommonTokenStream(lexer)
    let parser = new ModelParser(tokens)
    parser.buildParseTrees = true
    let tree = parser.lhs()
    this.visitLhs(tree)
    return this.varName
  }
  names() {
    // Expand dimensions in a subscripted var into individual vars with indices.
    if (this.modelLHSList.length > 0) {
      return this.modelLHSList
    } else {
      return [this.varName]
    }
  }
  visitLhs(ctx) {
    let varName = ctx.Id().getText()
    this.varName = varName
    super.visitLhs(ctx)
  }
  visitSubscriptList(ctx) {
    // Construct the modelLHSList array with the LHS expanded into an entry for each index
    // in the same format as Vensim log files.
    let subscripts = R.map(id => id.getText(), ctx.Id())
    this.modelSubscripts = subscripts.map(s => canonicalName(s))
    let modelLHSInds = dim => {
      // Construct the model indices for a dimension.
      // If the subscript range contains a dimension, expand it into index names in place.
      let indNames = R.map(subscriptModelName => {
        let subscript = canonicalName(subscriptModelName)
        if (isDimension(subscript)) {
          return sub(subscript).modelValue
        } else {
          return subscriptModelName
        }
      }, dim.modelValue)
      return R.flatten(indNames)
    }
    let expandLHSDims = (a, subscripts) => {
      // Recursively emit an LHS with Vensim names for each index in LHS dimensions.
      // Accumulate expanded subscripts in the "a" variable.
      if (subscripts.length === 0) {
        this.modelLHSList.push(`${this.varName}[${a.join(',')}]`)
      } else {
        // Expand the first subscript into the accumulator.
        let firstSub = canonicalName(R.head(subscripts))
        if (isDimension(firstSub)) {
          // Emit each index in a dimension subscript.
          for (let subscriptModelName of sub(firstSub).modelValue) {
            if (isDimension(canonicalName(subscriptModelName))) {
              // Expand a subdimension found in a dimension subscript value.
              for (let ind of modelLHSInds(sub(canonicalName(subscriptModelName)))) {
                expandLHSDims(a.concat(ind), R.tail(subscripts))
              }
            } else {
              // Expand an index subscript in a dimension directly.
              expandLHSDims(a.concat(subscriptModelName), R.tail(subscripts))
            }
          }
        } else {
          // Emit an index subscript directly.
          expandLHSDims(a.concat(R.head(subscripts)), R.tail(subscripts))
        }
      }
    }
    expandLHSDims([], subscripts)
    return this.modelLHSList
  }
}
