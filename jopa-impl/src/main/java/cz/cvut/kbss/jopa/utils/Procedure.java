package cz.cvut.kbss.jopa.utils;

/**
 * Generic procedure taking no arguments and returning no value.
 * <p>
 * Note: This is basically the same as {@link Runnable}, but the name corresponds better to the purpose of the interface
 * and doesn't get confused with threads.
 */
@FunctionalInterface
public interface Procedure {

    void execute();
}
