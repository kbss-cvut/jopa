package cz.cvut.kbss.jopa.model.query.criteria;

// TODO PRO - Javadoc
public interface Path<X> extends Expression<X>{
    <Y> Path<Y> getAttr(String attributeName);
    Path<?> getParentPath();
    Expression<Class<? extends X>> type();
}
