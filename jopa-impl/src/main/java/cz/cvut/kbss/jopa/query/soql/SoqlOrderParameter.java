package cz.cvut.kbss.jopa.query.soql;

public class SoqlOrderParameter extends SoqlParameter {

    private String orderingBy;

    private SoqlAttribute attribute;

    public SoqlOrderParameter(SoqlNode firstNode, String orderingBy) {
        super();
        this.setFirstNode(firstNode);
        this.orderingBy = orderingBy.isEmpty() ? "ASC" : orderingBy;
    }

    public String getOrderingBy() {
        return orderingBy;
    }

    public void setOrderingBy(String orderingBy) {
        this.orderingBy = orderingBy;
    }

    public SoqlAttribute getAttribute() {
        return attribute;
    }

    public void setAttribute(SoqlAttribute attribute) {
        this.attribute = attribute;
    }

    public String getOrderByPart() {
        String param = attribute.isFilter() ? getAsParam().substring(1) : attribute.getValue().substring(1);
        StringBuilder sb = new StringBuilder();
        if (orderingBy.equals("ASC")) {
            sb.append("?").append(param).append(" ");
        } else {
            sb.append("DESC(?").append(param).append(") ");
        }
        return sb.toString();
    }
}
