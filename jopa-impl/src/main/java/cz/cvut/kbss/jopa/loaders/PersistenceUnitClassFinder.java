package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.util.Objects;
import java.util.Set;

public class PersistenceUnitClassFinder {

    private final PersistenceUnitClassProcessor classProcessor = new PersistenceUnitClassProcessor();
    private final EntityLoader entityLoader = new EntityLoader();

    private boolean scanned = false;

    /**
     * Scans application classpath based on the {@link JOPAPersistenceProperties#SCAN_PACKAGE}, looking for classes
     * relevant for the persistence provider.
     * <p>
     * These classes include:
     * <p>
     * <ul> <li>Entities, i.e. classes annotated with {@link cz.cvut.kbss.jopa.model.annotations.OWLClass},</li>
     * <li>Result result mapping classes, i.e. classes annotated with {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping}
     * or {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMappings}</li> </ul>
     *
     * @param configuration Persistence configuration, should contain value for the {@link
     *                      JOPAPersistenceProperties#SCAN_PACKAGE} property
     * @throws IllegalArgumentException If {@link JOPAPersistenceProperties#SCAN_PACKAGE} values is missing
     */
    public void scanClasspath(Configuration configuration) {
        Objects.requireNonNull(configuration);
        if (!configuration.contains(JOPAPersistenceProperties.SCAN_PACKAGE)) {
            throw new IllegalArgumentException("Missing the " + JOPAPersistenceProperties.SCAN_PACKAGE + " property.");
        }
        String toScan = configuration.get(JOPAPersistenceProperties.SCAN_PACKAGE);
        if (toScan.isEmpty()) {
            throw new IllegalArgumentException(JOPAPersistenceProperties.SCAN_PACKAGE + " property cannot be empty.");
        }
        classProcessor.addListener(entityLoader);
        classProcessor.processClasses(toScan);
        this.scanned = true;
    }

    /**
     * Gets entity classes found during classpath scanning.
     *
     * @return Set of entity classes discovered on classpath
     */
    public Set<Class<?>> getEntities() {
        assert scanned;
        return entityLoader.getEntities();
    }
}
