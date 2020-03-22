package cz.cvut.kbss.jopa.query.soql;

public class SoqlOrderParameter extends SoqlParameter {

    private static final String ASC_ORDER = "ASC";
    private static final String DESC_ORDER = "DESC";

    private String orderingBy;

    private SoqlAttribute attribute;

    public SoqlOrderParameter(SoqlNode firstNode, String orderingBy) {
        super();
        this.setFirstNode(firstNode);
        this.orderingBy = orderingBy.isEmpty() ? ASC_ORDER : orderingBy;
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
        if (ASC_ORDER.equals(orderingBy)) {
            sb.append("?").append(param).append(" ");
        } else {
            sb.append(DESC_ORDER).append("(?").append(param).append(") ");
        }
        return sb.toString();
    }
}
