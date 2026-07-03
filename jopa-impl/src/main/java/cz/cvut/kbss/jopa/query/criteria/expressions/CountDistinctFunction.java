package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

public class CountDistinctFunction extends CountFunction {

    public CountDistinctFunction(AbstractPathExpression expression, CriteriaBuilder cb) {
        super(expression, cb);
    }

    @Override
    protected void constructFunctionArguments(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append("DISTINCT ");
        super.constructFunctionArguments(query, parameterFiller);
    }
}
