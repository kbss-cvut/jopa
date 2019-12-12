package cz.cvut.kbss.jopa.query.soql;

public class SoqlParam {

    private SoqlNode firstNode;

    public SoqlParam() {
    }

    public String getAsParam(){
        StringBuilder buildParam = new StringBuilder("?");
        buildParam.append(firstNode.getValue());
        SoqlNode pointer = firstNode;
        while (pointer.hasNextChild()) {
            pointer = pointer.getChild();
            buildParam.append(pointer.getCapitalizedvalue());
        }
        return buildParam.toString();
    }

    public SoqlNode getFirstNode() {
        return firstNode;
    }

    public void setFirstNode(SoqlNode firstNode) {
        this.firstNode = firstNode;
    }
}
