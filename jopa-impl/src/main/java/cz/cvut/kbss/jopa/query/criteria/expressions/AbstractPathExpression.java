package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.PathImpl;

abstract public class AbstractPathExpression<X> extends AbstractExpression<X> implements Path<X> {

    protected AbstractPathExpression pathSource;
    protected final Metamodel metamodel;

    public AbstractPathExpression(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel) {
        super(type);
        this.pathSource = pathSource;
        this.metamodel = metamodel;
    }

    //TODO - BAKALARKA - konzultacia - generika
    // new ExpressionAttributeImpl<>
//     Note: Applications using the string-based API may need to specify the type resulting from the get operation in order to avoid the use of Path variables.
//     For example:
//     CriteriaQuery<Person> q = cb.createQuery(Person.class);
//     Root<Person> p = q.from(Person.class);
//     q.select(p)
//     .where(cb.isMember("joe",
//     p.<Set<String>>get("nicknames")));
//     rather than:
//     CriteriaQuery<Person> q = cb.createQuery(Person.class);
//     Root<Person> p = q.from(Person.class);
//     Path<Set<String>> nicknames = p.get("nicknames");
//     q.select(p)
//     .where(cb.isMember("joe", nicknames));
    public <Y> Path<Y> getAttr(String attributeName) throws IllegalArgumentException {
        Attribute attribute = metamodel.entity(type).getAttribute(attributeName);
        Path<Y> newPathSource = new PathImpl<Y>(this.metamodel,new ExpressionAttributeImpl(type,  this.pathSource, this.metamodel, attribute),null);
        //TODO - BAKALARKA - konzultacia - generika
        //new ExpressionAttributeImpl< >
        return newPathSource;
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        this.pathSource = new ExpressionAttributeImpl(type,  this.pathSource, this.metamodel, attribute);
        return this.pathSource;
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}


