package cz.cvut.kbss.ontodriver.impl.utils;

import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.hibernate.HibernateException;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.profiles.OWL2DLProfile;
import org.semanticweb.owlapi.profiles.OWL2ELProfile;
import org.semanticweb.owlapi.profiles.OWL2Profile;
import org.semanticweb.owlapi.profiles.OWL2QLProfile;
import org.semanticweb.owlapi.profiles.OWL2RLProfile;
import org.semanticweb.owlapi.profiles.OWLProfile;
import org.semanticweb.owlapi.profiles.OWLProfileReport;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.ontodriver.ContextExpressiveness;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiUtils;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntology;
import de.fraunhofer.iitb.owldb.OWLDBOntologyManager;

public final class OntologyProfileChecker {

	private static final Logger LOG = Logger.getLogger(OntologyProfileChecker.class.getName());

	private static final List<OWLProfile> PROFILES = initProfiles();
	private static final Map<Class<? extends OWLProfile>, ContextExpressiveness> PROFILE_NAMES = initProfileNames();

	/**
	 * Private constructor.
	 */
	private OntologyProfileChecker() {
		throw new AssertionError("Cannot instantiante OntologyProfileChecker.");
	}

	/**
	 * Resolves ontology profiles for contexts specified by
	 * {@code contextProperties}. </p>
	 * 
	 * The {@code OntologyStorageProperties} instances associated with
	 * {@code Context} instances are used to find the physical location of the
	 * storage so that its profile can be determined. </p>
	 * 
	 * The profile expressiveness is set directly on the {@code Context}
	 * instances in the argument.
	 * 
	 * @param storageProperties
	 *            Contexts with storage properties
	 */
	public static void checkProfiles(final OntologyStorageProperties storageProperties) {
		if (storageProperties == null) {
			throw new NullPointerException();
		}
		final OWLOntologyManager fileM = OWLManager.createOWLOntologyManager();
		final OWLOntologyManager owldbM = OWLDBManager.createOWLOntologyManager(OWLDataFactoryImpl
				.getInstance());
		switch (storageProperties.getConnectorType()) {
		case OWLAPI:
			switch (DriverOwlapiFactory.resolveStorageType(storageProperties)) {
			case FILE:
				resolveFileProfile(storageProperties, fileM);
				break;
			case OWLDB:
				// TODO Resolving doesn't work for larger ontologies, OWLDB
				// throws NPX
				resolveOwldbProfile(storageProperties, owldbM);
				OWLDBManager.getHibernateProvider().releaseSessionFactory(owldbM,
						storageProperties.getPhysicalURI().toString());
				break;
			}
			break;
		case JENA:
			// TODO
			break;
		case OWLIM:
			// TODO
			break;
		case SESAME:
			// TODO
			break;
		default:
			break;
		}
	}

	private static void resolveFileProfile(OntologyStorageProperties props, OWLOntologyManager m) {
		OWLOntology o;
		try {
			o = m.loadOntologyFromOntologyDocument(IRI.create(props.getPhysicalURI()));
		} catch (OWLOntologyCreationException e) {
			if (e.getCause() instanceof FileNotFoundException) {
				LOG.warning("Ontology in location " + props.getPhysicalURI()
						+ " does not exist yet. Setting profile to OWL 2.");
			} else {
				LOG.log(Level.SEVERE,
						"Unable to load ontology from location " + props.getPhysicalURI(), e);
			}
			return;
		}
		resolveProfile(o);
	}

	private static void resolveOwldbProfile(OntologyStorageProperties props, OWLOntologyManager m) {
		OWLOntology o;
		try {
			final Properties p = new Properties();
			OwlapiUtils.initHibernateProperties(p, props);
			OwlapiUtils.setOntologyManagerIriMapper(m, props.getOntologyURI(),
					props.getPhysicalURI());
			o = ((OWLDBOntologyManager) m).loadOntology(IRI.create(props.getOntologyURI()), p);
		} catch (OWLOntologyCreationException e) {
			LOG.log(Level.WARNING,
					"Unable to load ontology from location " + props.getPhysicalURI()
							+ ". The ontology may not exist yet, setting profile to OWL 2.", e);
			return;
		} catch (HibernateException e) {
			LOG.warning("Unable to load ontology from location " + props.getPhysicalURI()
					+ ". The ontology may not exist yet, setting profile to OWL 2.");
			LOG.config("Exception message: " + e.getMessage());
			return;
		}
		resolveProfile(o);
		((OWLDBOntology) o).destroyConnection();
	}

	private static void resolveProfile(OWLOntology o) {
		for (OWLProfile p : PROFILES) {
			final OWLProfileReport r = p.checkOntology(o);
			if (r.isInProfile()) {
				return;
			}
		}
		LOG.warning("The ontology with does not correspond to any supported OWL profile.");
	}

	private static List<OWLProfile> initProfiles() {
		return Arrays.asList(new OWL2Profile(), new OWL2DLProfile(), new OWL2ELProfile(),
				new OWL2RLProfile(), new OWL2QLProfile());
	}

	private static Map<Class<? extends OWLProfile>, ContextExpressiveness> initProfileNames() {
		final Map<Class<? extends OWLProfile>, ContextExpressiveness> map = new HashMap<Class<? extends OWLProfile>, ContextExpressiveness>();
		map.put(OWL2Profile.class, ContextExpressiveness.OWL2FULL);
		map.put(OWL2DLProfile.class, ContextExpressiveness.OWL2DL);
		map.put(OWL2ELProfile.class, ContextExpressiveness.OWL2EL);
		map.put(OWL2QLProfile.class, ContextExpressiveness.OWL2QL);
		map.put(OWL2RLProfile.class, ContextExpressiveness.OWL2RL);
		return map;
	}
}
