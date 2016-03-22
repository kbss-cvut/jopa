package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

/**
 * Procedure functional interface.
 * <p>
 * Executes a method which takes no arguments and returns no result.
 */
@FunctionalInterface
interface Procedure {
    /**
     * Executes the procedure.
     *
     * @throws SesameDriverException
     */
    void execute() throws SesameDriverException;
}
