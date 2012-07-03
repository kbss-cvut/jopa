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

package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import cz.cvut.kbss.owlpersistence.Persistence;
import cz.cvut.kbss.owlpersistence.model.EntityManager;

@Ignore
public class TestEnvironment {
	public static final Logger log = Logger.getLogger(TestEnvironment.class
			.getName());

	public static String dir = "testResults";

	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	// private static final String REASONER_FACTORY_CLASS =
	// "org.semanticweb.HermiT.Reasoner$ReasonerFactory";

	public static EntityManager getPersistenceConnector(String name) {
		return getPersistenceConnector(name, false, true);
	}

	public static EntityManager getPersistenceConnector(String name,
			boolean cache) {
		return getPersistenceConnector(name, false, cache);
	}

	public static EntityManager getPersistenceConnector(String name,
			boolean db, boolean cache) {
		try {
			final Map<String, String> params = new HashMap<String, String>();

			final IRI iri = IRI
					.create("http://krizik.felk.cvut.cz/ontologies/2009/owlpersistence-tests/"
							+ name);
			final File url = new File(dir + "/" + name + ".owl");
			if (!db) {
				// Ontology stored in file
//				final OWLOntologyManager m = OWLManager
//						.createOWLOntologyManager();
//
//				OWLOntology o = m.createOntology(iri);
//
//				m.saveOntology(o, IRI.create(url.toURI()));
				params.put(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY,
						iri.toString());
				params.put(
						OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
						url.toString());
				// The ONTOLOGY_URI_KEY should be the iri
				// and the url should be set as ONTOLOGY_PHYSICAL_LOCATION
				// The ontology creation should be done in the accessor
			} else {
				// OWLDB ontology access
				final String dbUri = "jdbc:postgresql://localhost/owldb";
				params.put(
						OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
						dbUri);
				params.put(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY,
						iri.toString());
			}

			params.put("javax.persistence.provider",
					EntityManagerFactoryImpl.class.getName());
			if (cache) {
				params.put("cache", "on");
			} else {
				params.put("cache", "off");
			}
			/* Set location of the entities (package) */
			params.put("location", "cz.cvut.kbss.owlpersistence.owlapi");
			params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
					OWLAPIPersistenceProvider.class.getName());
			// params.put(OWLAPIPersistenceProperties.ONTOLOGY_FILE_KEY, url
			// .getAbsolutePath());
			params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
					REASONER_FACTORY_CLASS);

			return Persistence.createEntityManagerFactory("context-name",
					params).createEntityManager();
//		} catch (OWLOntologyCreationException e) {
//			log.log(Level.SEVERE, e.getMessage(), e);
		} catch (UnknownOWLOntologyException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
//		} catch (OWLOntologyStorageException e) {
//			log.log(Level.SEVERE, e.getMessage(), e);
		}
		return null;
	}

	public static Logger getLogger() {
		return log;
	}
}
