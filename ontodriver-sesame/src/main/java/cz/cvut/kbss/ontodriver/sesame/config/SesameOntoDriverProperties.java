/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.config;

public abstract class SesameOntoDriverProperties {

    /**
     * Specifies whether a in-memory storage should be used for local Sesame repositories.
     * <p>
     * When set to true, any local Sesame repositories that are created by the
     * driver are created as only MemoryStores without any persistent backend.
     * Repositories accessed over the Internet or already existing locally are
     * not affected by this setting.
     * <p>
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_VOLATILE_STORAGE = "cz.cvut.kbss.ontodriver.sesame.use-volatile-storage";

    /**
     * Specifies whether Sesame inference (RDFS, forward chaining) should be used.
     * <p>
     * Note that this setting applies only to local storages (in memory or native), remote storages use their own
     * inference settings.
     * <p>
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_INFERENCE = "cz.cvut.kbss.ontodriver.sesame.use-inference";

    /**
     * Specifies how many requested assertions suffice to perform load all.
     * <p>
     * More specifically, if the number of assertions requested by an {@link cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor}
     * is low enough, the statements for them will be loaded by asking Sesame for statements with subject and property
     * bound.
     * <p>
     * Otherwise, statements will be loaded using only subject bound and will be filtered according to the assertions.
     * This will in most cases have better performance than loading with bound property.
     */
    public static final String SESAME_LOAD_ALL_THRESHOLD = "cz.cvut.kbss.ontodriver.sesame.load-all-threshold";

    /**
     * Path to repository configuration file.
     * <p>
     * The configuration file is a Turtle file corresponding to the repository config file schema defined by RDF4J.
     * See for example <a href="http://docs.rdf4j.org/server-workbench-console/#_repository_configuration_templates_advanced">
     * http://docs.rdf4j.org/server-workbench-console/#_repository_configuration_templates_advanced</a>
     * <p>
     * Sample configuration files are available in the RDF4J GitHub repo:
     * <a href="https://github.com/eclipse/rdf4j/tree/master/repository/api/src/main/resources/org/eclipse/rdf4j/repository/config">
     * https://github.com/eclipse/rdf4j/tree/master/repository/api/src/main/resources/org/eclipse/rdf4j/repository/config</a>
     * <p>
     * The config file can be present on the classpath (if the value starts with the {@code classpath:} prefix) or can be loaded
     * using the value as file path.
     * The loaded configuration supersedes relevant properties passed to the driver, e.g., {@link #SESAME_USE_VOLATILE_STORAGE} or {@link #SESAME_USE_INFERENCE}.
     * <p>
     * Note that the config applies only to embedded repositories created by the driver, repositories on a Sesame/RDF4J server
     * to which the driver just connects must preexist and the configuration does not apply to them. The physical URI specified
     * in configuration must correspond to the URI of the repository in the config file, i.e., for memory store, the repository ID must
     * be the same. For a native store, the physical URI must be in the form {@code /local-path/repositories/repository-id}, where {@code local-path} will
     * be used for initialization of a local {@link org.eclipse.rdf4j.repository.manager.RepositoryManager} and {@code repository-id} must again
     * correspond to the repository ID in the configuration file.
     */
    public static final String SESAME_REPOSITORY_CONFIG = "cz.cvut.kbss.ontodriver.sesame.repository-config";

    private SesameOntoDriverProperties() {
        throw new AssertionError();
    }
}
