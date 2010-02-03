package cz.cvut.kbss.owlpersistence.model.query.criteria;

/**
 * An object that defines an ordering over the query results.
 */
public interface Order {
	/**
	 * Switch the ordering.
	 * 
	 * @return a new Order instance with the reversed ordering
	 */
	Order reverse();

	/**
	 * Whether ascending ordering is in effect.
	 * 
	 * @return boolean indicating whether ordering is ascending
	 */
	boolean isAscending();

	/**
	 * Return the expression that is used for ordering.
	 * 
	 * @return expression used for ordering
	 */
	Expression<?> getExpression();
}
