package cz.cvut.kbss.jopa.query;

import java.util.Collection;

/**
 * Represents a caretaker of a query, enabling parameter setting and final assembly of the query.
 *
 * @author kidney
 */
public interface QueryHolder {

    /**
     * Gets a collection of parameters (their names) in the query.
     *
     * @return Parameter names
     */
    Collection<String> getParameters();

    /**
     * Gets value bound to the parameter with specified name.
     *
     * @param name Parameter name
     * @return parameter value
     * @throws IllegalArgumentException If there is no parameter with the specified name
     */
    Object getParameterValue(String name);

    /**
     * Gets value bound to the parameter at the specified position.
     *
     * @param position position
     * @return parameter value
     * @throws IllegalArgumentException If the position is not present in the query
     */
    Object getParameterValue(int position);

    /**
     * Sets value of the specified parameter in the query.
     * <p>
     * If a value was already specified for the parameter, it is overwritten by the new one.
     *
     * @param parameter Parameter name
     * @param value     Value to use
     * @throws IllegalArgumentException If there is no such parameter in the query
     */
    void setParameter(String parameter, ParameterValue value);

    /**
     * Clears any previously set value of the specified parameter.
     *
     * @param parameter Parameter name
     * @throws IllegalArgumentException If there is no such parameter in the query
     */
    void clearParameter(String parameter);

    /**
     * Clears any previously set parameter values in this query.
     */
    void clearParameters();

    /**
     * Assembles the query, using any parameter values specified, and returns it as a string.
     *
     * @return Assembled query
     */
    String assembleQuery();
}
