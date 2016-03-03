package cz.cvut.kbss.ontodriver.owlapi.config;

public abstract class OwlapiOntoDriverProperties {

    private OwlapiOntoDriverProperties() {
        throw new AssertionError();
    }

    /**
     * Represents location of a OWLAPI mapping file.
     * <p>
     * This file is used to map logical IRIs of ontologies to their physical locations, e.g. in case they are not
     * accessible online, but are stored in files locally.
     */
    public static final String MAPPING_FILE_LOCATION = "cz.cvut.kbss.ontodriver.owlapi.mappingFile";

    /**
     * Delimits IRI mappings in the mapping file.
     *
     * @see #MAPPING_FILE_LOCATION
     */
    public static final String IRI_MAPPING_DELIMITER = "cz.cvut.kbss.ontodriver.owlapi.mapping-delimiter";

    /**
     * Default IRI mapping delimiter.
     *
     * @see #IRI_MAPPING_DELIMITER
     */
    public static final String DEFAULT_IRI_MAPPING_DELIMITER = ">";
}
