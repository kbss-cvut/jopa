/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.model.PersistenceProperties;

public interface OWLAPIPersistenceProperties extends PersistenceProperties {

    String ONTOLOGY_URI_KEY = "cz.cvut.jopa.ontologyURI";
    String ONTOLOGY_DB_CONNECTION = "cz.cvut.jopa.ontologyDBConnection";
    String ONTOLOGY_FILE_KEY = "cz.cvut.jopa.ontologyDocumentURI";
    String ONTOLOGY_PHYSICAL_URI_KEY = "cz.cvut.jopa.ontologyPhysicalURI";
    String MAPPING_FILE_URI_KEY = "cz.cvut.jopa.mappingFileURI";
    String REASONER_FACTORY_CLASS = "cz.cvut.jopa.reasonerFactoryClass";
    String LANG = "cz.cvut.jopa.lang";

    /**
     * Whether a second level cache should be used.
     */
    String CACHE_ENABLED = "cz.cvut.jopa.cache.enable";

    /**
     * Where the entity classes are located.
     */
    String SCAN_PACKAGE = "cz.cvut.jopa.scanPackage";

    /**
     * Cached entity time to live. In seconds.
     */
    String CACHE_TTL = "cz.cvut.jopa.cache.ttl";

    /**
     * How often should the cache be swept for dead entities. In seconds.
     */
    String CACHE_SWEEP_RATE = "cz.cvut.jopa.cache.sweepRate";

    /**
     * Type of the second level cache. Currently supported are {@literal ttl} and {@literal lru}.
     */
    String CACHE_TYPE = "cz.cvut.jopa.cache.type";

    /**
     * Capacity of the LRU second level cache.
     */
    String LRU_CACHE_CAPACITY = "cz.cvut.jopa.cache.lru.capacity";

    /**
     * Disable integrity constraints validation on entity/field load.
     */
    String DISABLE_IC_VALIDATION_ON_LOAD = "cz.cvut.jopa.ic.validation.disableOnLoad";
}
