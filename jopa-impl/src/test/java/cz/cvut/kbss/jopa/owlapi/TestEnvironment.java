/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.owlapi;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;

@Ignore
public class TestEnvironment {
	private static final Logger log = Logger.getLogger(TestEnvironment.class.getName());

	public static final String dir = "testResults";
	public static final String DB_URI = "jdbc:postgresql://localhost/owldb";
	public static final String DB_USERNAME = "owldb";
	public static final String DB_PASSWORD = "owldb";
	public static final String DB_DRIVER = "org.postgresql.Driver";

	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	/**
	 * True if the ontology file should be deleted before access to it is
	 * initialized. This effectively means that the test will create the
	 * ontology from scratch. Default value is true.
	 */
	public static boolean shouldDeleteOntologyFile = true;

	// private static final String REASONER_FACTORY_CLASS =
	// "org.semanticweb.HermiT.Reasoner$ReasonerFactory";

	public static enum Storage {
		OWLDB, FILE
	};

	public static EntityManager getPersistenceConnector(String name) {
		return getPersistenceConnector(name, Storage.FILE, true);
	}

	public static EntityManager getPersistenceConnector(String name, boolean cache) {
		return getPersistenceConnector(name, Storage.FILE, cache);
	}

	public static EntityManager getPersistenceConnector(String name, Storage storage, boolean cache) {
		final Map<String, String> params = new HashMap<String, String>();
		final IRI iri = IRI.create("http://krizik.felk.cvut.cz/ontologies/2009/jopa-tests/" + name);
		URI physicalUri = null;
		try {
			switch (storage) {
			case FILE:
				// Ontology stored in a file
				final File url = new File(dir + "/" + name + ".owl");
				if (url.exists() && shouldDeleteOntologyFile) {
					url.delete();
				}
				physicalUri = url.toURI();
				break;
			case OWLDB:
				// OWLDB ontology access
				physicalUri = URI.create(DB_URI);
			}
			final List<OntologyStorageProperties> storageProps = createOwlapiStorageProperties(
					iri.toURI(), physicalUri, storage);
			params.put("javax.persistence.provider", EntityManagerFactoryImpl.class.getName());
			if (cache) {
				params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "on");
			} else {
				params.put(OWLAPIPersistenceProperties.CACHE_PROPERTY, "off");
			}
			/* Set location of the entities (package) */
			params.put("location", "cz.cvut.kbss.jopa.owlapi");
			params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
					OWLAPIPersistenceProvider.class.getName());
			// params.put(OWLAPIPersistenceProperties.ONTOLOGY_FILE_KEY, url
			// .getAbsolutePath());
			params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS, REASONER_FACTORY_CLASS);

			return Persistence.createEntityManagerFactory("context-name", storageProps, params)
					.createEntityManager();
		} catch (UnknownOWLOntologyException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}
		return null;
	}

	private static List<OntologyStorageProperties> createOwlapiStorageProperties(URI ontologyUri,
			URI physicalUri, Storage storage) {
		final List<OntologyStorageProperties> props = new ArrayList<OntologyStorageProperties>(1);
		OntologyStorageProperties p = null;
		switch (storage) {
		case FILE:
			p = new OntologyStorageProperties(ontologyUri, physicalUri,
					OntologyConnectorType.OWLAPI);
			break;
		case OWLDB:
			p = OwldbOntologyStorageProperties.ontologyUri(ontologyUri).physicalUri(physicalUri)
					.connectorType(OntologyConnectorType.OWLAPI).username(DB_USERNAME)
					.password(DB_PASSWORD).jdbcDriverClass(DB_DRIVER).build();
			break;
		}
		props.add(p);
		return props;
	}

	public static Logger getLogger() {
		return log;
	}
}
