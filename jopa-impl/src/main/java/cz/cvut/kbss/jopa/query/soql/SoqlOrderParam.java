package cz.cvut.kbss.jopa.query.soql;

public class SoqlOrderParam {

    private SoqlNode firstNode;

    private String orderingBy ;

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
        if(orderingBy.equals("ASC")){
            return getAsParam();
        }else{
            StringBuilder sb = new StringBuilder("DESC(");
            return sb.append(getAsParam()).append(")").toString();
        }
    }
}
