/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Scans classpath to discover classes relevant to persistence unit building.
 * <p>
 * Only classes under the package configured via {@link JOPAPersistenceProperties#SCAN_PACKAGE} are processed.
 */
public class PersistenceUnitClassFinder {

    private final EntityLoader entityLoader = new EntityLoader();
    private final ResultSetMappingLoader resultSetMappingLoader = new ResultSetMappingLoader();
    private final ConverterLoader converterLoader = new ConverterLoader();

    private boolean scanned = false;

    /**
     * Scans application classpath based on the {@link JOPAPersistenceProperties#SCAN_PACKAGE}, looking for classes
     * relevant for the persistence provider.
     * <p>
     * These classes include:
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
        String scanPackageConfig = configuration.get(JOPAPersistenceProperties.SCAN_PACKAGE, "");
        final String[] toScan = scanPackageConfig.split(",");
        final ClasspathScanner classpathScanner = resolveClasspathScanner(configuration);
        classpathScanner.addListener(entityLoader);
        classpathScanner.addListener(resultSetMappingLoader);
        classpathScanner.addListener(converterLoader);
        Stream.of(toScan).map(String::trim).forEach(classpathScanner::processClasses);
        this.scanned = true;
    }

    private static ClasspathScanner resolveClasspathScanner(Configuration config) {
        try {
            final String scannerType = config.get(JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS,
                                                  DefaultClasspathScanner.class.getName());
            final Class<?> scannerCls = Class.forName(scannerType);
            return (ClasspathScanner) ReflectionUtils.instantiateUsingDefaultConstructor(scannerCls);
        } catch (ClassNotFoundException | cz.cvut.kbss.jopa.exception.InstantiationException e) {
            throw new MetamodelInitializationException("Unable to instantiate configured ClasspathScanner.", e);
        }
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

    /**
     * Gets {@link SparqlResultSetMapping}s found during classpath scanning.
     *
     * @return Set of result set mapping annotations discovered on classpath
     */
    public Set<SparqlResultSetMapping> getResultSetMappings() {
        assert scanned;
        return resultSetMappingLoader.getMappings();
    }

    /**
     * Gets {@link cz.cvut.kbss.jopa.model.AttributeConverter} implementations found during classpath scanning.
     *
     * The converters are wrapped in an internal helper class {@link ConverterWrapper}.
     * @return Set of custom converters
     */
    public Map<Class<?>, ConverterWrapper<?, ?>> getAttributeConverters() {
        assert scanned;
        return converterLoader.getConverters();
    }
}
