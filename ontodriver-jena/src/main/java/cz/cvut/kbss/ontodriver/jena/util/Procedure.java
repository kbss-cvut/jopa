package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

/**
 * Simple procedure which does accepts no parameter and returns nothing.
 * <p>
 * Similar to {@link Runnable}, but throws checked exception.
 */
@FunctionalInterface
public interface Procedure {

    void execute() throws JenaDriverException;
}
