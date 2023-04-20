package cz.cvut.kbss.jopa.query.soql;

abstract class SoqlNode {

    SoqlNode parent;
    SoqlNode child;

    SoqlNode() {
    }

    SoqlNode(SoqlNode parent) {
        this.parent = parent;
    }

    public boolean hasChild() {
        return child != null;
    }

    public SoqlNode getChild() {
        return child;
    }

    public SoqlNode getParent() {
        return parent;
    }

    public void setChild(SoqlNode child) {
        this.child = child;
    }

    public void setParent(SoqlNode parent) {
        this.parent = parent;
    }

    public abstract String getValue();

    public abstract void setValue(String value);

    public abstract String getCapitalizedValue();

    public abstract String getIri();

    public abstract void setIri(String iri);

    public abstract boolean requiresFilterExpression();

    public abstract String getFilterExpression(String filterParam, String filterValue);
}
