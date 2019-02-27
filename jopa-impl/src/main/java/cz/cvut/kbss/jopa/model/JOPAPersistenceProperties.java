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
package cz.cvut.kbss.jopa.model;

public final class JOPAPersistenceProperties extends PersistenceProperties {

    /**
     * Logical URI of the underlying ontology.
     */
    public static final String ONTOLOGY_URI_KEY = "cz.cvut.jopa.ontology.logicalUri";

    /**
     * Physical location of the underlying ontology storage.
     */
    public static final String ONTOLOGY_PHYSICAL_URI_KEY = "cz.cvut.jopa.ontology.physicalURI";

    /**
     * Class name of the OntoDriver implementation.
     */
    public static final String DATA_SOURCE_CLASS = "cz.cvut.jopa.dataSource.class";

    /**
     * Ontology language.
     */
    public static final String LANG = "cz.cvut.jopa.lang";

    /**
     * Whether a second level cache should be used.
     */
    public static final String CACHE_ENABLED = "cz.cvut.jopa.cache.enable";

    /**
     * Where the entity classes are located.
     */
    public static final String SCAN_PACKAGE = "cz.cvut.jopa.scanPackage";

    /**
     * Cached entity time to live. In seconds.
     */
    public static final String CACHE_TTL = "cz.cvut.jopa.cache.ttl";

    /**
     * How often should the cache be swept for dead entities. In seconds.
     */
    public static final String CACHE_SWEEP_RATE = "cz.cvut.jopa.cache.sweepRate";

    /**
     * Type of the second level cache. Currently supported are {@literal ttl} and {@literal lru}.
     */
    public static final String CACHE_TYPE = "cz.cvut.jopa.cache.type";

    /**
     * Capacity of the LRU second level cache.
     */
    public static final String LRU_CACHE_CAPACITY = "cz.cvut.jopa.cache.lru.capacity";

    /**
     * Disable integrity constraints validation on entity/field load.
     */
    public static final String DISABLE_IC_VALIDATION_ON_LOAD = "cz.cvut.jopa.ic.validation.disableOnLoad";

    private JOPAPersistenceProperties() {
        throw new AssertionError();
    }
}
