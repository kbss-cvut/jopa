package cz.cvut.kbss.jopa.loaders;

import java.util.function.Consumer;

/**
 * Scans application classpath, allowing to find classes of significance (e.g., entity classes).
 */
public interface ClasspathScanner {

    /**
     * Registers a listener to which discovered classes will be passed for processing.
     *
     * @param listener Listener to invoke with discovered classes
     */
    void addListener(Consumer<Class<?>> listener);

    /**
     * Start class processing, looking for classes in the specified package (and its descendants).
     *
     * @param scanPackage Package to scan
     */
    void processClasses(String scanPackage);
}
