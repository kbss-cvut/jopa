package cz.cvut.kbss.owlpersistence.model.query.criteria;

import cz.cvut.kbss.owlpersistence.model.query.Parameter;

/**
 * Type of criteria query parameter expressions.
 * 
 * @param <T>
 *            the type of the parameter expression
 */
public interface ParameterExpression<T> extends Parameter<T>, Expression<T> {
}
