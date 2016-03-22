package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

/**
 * Defines a procedure interface, which takes no arguments and returns nothing.
 */
@FunctionalInterface
public interface Procedure {
    void execute() throws OwlapiDriverException;
}
