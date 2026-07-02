package cz.cvut.kbss.jopa.query.soql;

/**
 * Captures the kind of projection in the SOQL SELECT clause.
 *
 * @param aggregateFunction the aggregate function wrapping the projected variable, or {@code null} for a plain
 *                          projection
 * @param distinct          whether the outer {@code SELECT DISTINCT} modifier is present
 * @param aggregateDistinct whether {@code DISTINCT} is present inside the aggregate argument
 */
record SelectProjection(AggregateFunction aggregateFunction, boolean distinct, boolean aggregateDistinct) {

    SelectProjection withAggregateFunction(AggregateFunction fn) {
        return new SelectProjection(fn, distinct, aggregateDistinct);
    }

    SelectProjection withDistinct(boolean value) {
        return new SelectProjection(aggregateFunction, value, aggregateDistinct);
    }

    SelectProjection withAggregateDistinct(boolean value) {
        return new SelectProjection(aggregateFunction, distinct, value);
    }
}
