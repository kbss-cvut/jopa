package cz.cvut.kbss.jopa.query.soql;

public class SoqlGroupParameter extends SoqlParameter {

    private SoqlAttribute attribute;

    public SoqlGroupParameter(SoqlNode firstNode) {
        setFirstNode(firstNode);
    }

    public SoqlAttribute getAttribute() {
        return attribute;
    }

    public void setAttribute(SoqlAttribute attribute) {
        this.attribute = attribute;
    }

    public String getGroupByPart(){
        String param = attribute.isFilter() ? getAsParam().substring(1) : attribute.getValue().substring(1);
        return "?" + param + " ";
    }
}
