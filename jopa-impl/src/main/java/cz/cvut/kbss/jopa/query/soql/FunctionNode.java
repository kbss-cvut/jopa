package cz.cvut.kbss.jopa.query.soql;

class FunctionNode extends SoqlNode {

    // SOQL function name
    private final String functionName;

    FunctionNode(SoqlNode child, String functionName) {
        assert child != null;
        assert functionName != null && !functionName.isEmpty();
        this.child = child;
        this.functionName = functionName;
    }

    @Override
    public boolean hasChild() {
        return child.hasChild();
    }

    @Override
    public SoqlNode getChild() {
        return child.getChild();
    }

    @Override
    public void setChild(SoqlNode child) {
        child.setChild(child);
    }

    @Override
    public String getValue() {
        return child.getValue();
    }

    @Override
    public void setValue(String value) {
        child.setValue(value);
    }

    @Override
    public String getCapitalizedValue() {
        return child.getCapitalizedValue();
    }

    @Override
    public String getIri() {
        return child.getIri();
    }

    @Override
    public void setIri(String iri) {
        child.setIri(iri);
    }

    @Override
    public boolean requiresFilterExpression() {
        return true;
    }

    @Override
    public String toFilterExpression(String filterParam, String filterValue) {
        return SoqlFunctionTranslator.getSparqlFunction(functionName) + "(" + child.toFilterExpression(filterParam,
                                                                                                       filterValue) + ")";
    }
}
