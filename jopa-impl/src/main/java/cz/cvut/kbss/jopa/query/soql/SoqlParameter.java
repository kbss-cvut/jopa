package cz.cvut.kbss.jopa.query.soql;

public class SoqlParameter {

    private SoqlNode firstNode;

    public SoqlParameter() {
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

    public String getAsValue(){
        StringBuilder buildParam = new StringBuilder("?");
        SoqlNode firstNode = getFirstNode();
        SoqlNode pointer;
        if(firstNode.hasNextChild()){
            pointer = getFirstNode().getChild();
        } else {
            return "?x";
        }
        buildParam.append(pointer.getValue());
        while (pointer.hasNextChild()) {
            pointer = pointer.getChild();
            buildParam.append(pointer.getCapitalizedvalue());
        }
        return buildParam.toString();
    }
}
