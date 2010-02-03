package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.PersistenceProperties;

public interface OWLAPIPersistenceProperties extends PersistenceProperties {

	public static final String ONTOLOGY_URI_KEY = "cz.cvut.owlpersistence.ontologyURI";
	public static final String ONTOLOGY_DB_CONNECTION = "cz.cvut.owlpersistence.ontologyDBConnection";
	public static final String ONTOLOGY_FILE_KEY = "cz.cvut.owlpersistence.ontologyDocumentURI";
	public static final String MAPPING_FILE_URI_KEY = "cz.cvut.owlpersistence.mappingFileURI";
	public static final String REASONER_FACTORY_CLASS = "cz.cvut.owlpersistence.reasonerFactoryClass";
	public static final String USE_OLD_OWLAPIV3 = "cz.cvut.owlpersistence.useoldowlapiv3";
	public static final String LANG = "cz.cvut.owlpersistence.lang";
}
