package cz.cvut.kbss.jopa.query.soql;

public class SoqlOrderParam {

    private SoqlNode firstNode;

    private String orderingBy ;

    private SoqlAttribute attribute;

    public SoqlOrderParam(SoqlNode firstNode, String orderingBy) {
        this.firstNode = firstNode;
        this.orderingBy = orderingBy.isEmpty() ? "ASC" : orderingBy;
    }

    public SoqlNode getFirstNode() {
        return firstNode;
    }

    public void setFirstNode(SoqlNode firstNode) {
        this.firstNode = firstNode;
    }

    public String getOrderingBy() {
        return orderingBy;
    }

    public void setOrderingBy(String orderingBy) {
        this.orderingBy = orderingBy;
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

    public String getOrderByPart(){
        String param = attribute.isFilter() ? getAsParam() : attribute.getValue().substring(1);
        StringBuilder sb = new StringBuilder();
        if(orderingBy.equals("ASC")){
            sb.append("?").append(param);
        }else{
            sb.append("DESC(").append(param).append(")");
        }
        return sb.toString();
    }

    public SoqlAttribute getAttribute() {
        return attribute;
    }

    public void setAttribute(SoqlAttribute attribute) {
        this.attribute = attribute;
    }
}
