function codegen(astString){
  var parsedAst = JSON.parse(astString);
  var result = escodegen.generate(parsedAst);
  return result;
}