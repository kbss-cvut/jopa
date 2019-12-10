package cz.cvut.kbss.jopa.query.soql;

public class SoqlNode {

    private SoqlNode parent;
    private SoqlNode child;
    private String value;

    public SoqlNode(SoqlNode parent, SoqlNode child, String value) {
        this.parent = parent;
        this.child = child;
        this.value = value;
    }

    public SoqlNode(String value){
        this.value = value;
    }

    public SoqlNode(SoqlNode parent, String value){
        this.parent = parent;
        this.value = value;
    }



    public boolean hasNextChild(){
        return this.child != null;
    }

    public SoqlNode getChild(){
        return this.child;
    }

    public boolean hasNextParent(){
        return this.parent != null;
    }

    public SoqlNode getParent(){
        return this.parent;
    }

    public String getValue(){
        return this.value;
    }

    public String getCapitalizedvalue(){
        return this.value.substring(0, 1).toUpperCase() + this.value.substring(1);
    }

    public void setChild(SoqlNode child) {
        this.child = child;
    }

    public void setParent(SoqlNode parent) {
        this.parent = parent;
    }
}
