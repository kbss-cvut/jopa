package cz.cvut.kbss.ontodriver.impl.utils;

import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

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

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.ContextExpressiveness;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
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
	 * @param contextProperties
	 *            Contexts with storage properties
	 */
	public static void checkProfiles(final Map<Context, OntologyStorageProperties> contextProperties) {
		if (contextProperties == null) {
			throw new NullPointerException();
		}
		if (contextProperties.isEmpty()) {
			return;
		}
		final OWLOntologyManager fileM = OWLManager.createOWLOntologyManager();
		final OWLOntologyManager owldbM = OWLDBManager.createOWLOntologyManager(null);
		for (Entry<Context, OntologyStorageProperties> e : contextProperties.entrySet()) {
			final OntologyStorageProperties storageProps = e.getValue();
			switch (storageProps.getConnectorType()) {
			case OWLAPI:
				switch (DriverOwlapiFactory.resolveStorageType(storageProps)) {
				case FILE:
					resolveFileProfile(e.getKey(), storageProps, fileM);
					break;
				case OWLDB:
					resolveOwldbProfile(e.getKey(), storageProps, owldbM);
					break;
				}
				break;
			case JENA:
				// TODO
				break;
			}
		}
	}

	private static void resolveFileProfile(Context ctx, OntologyStorageProperties props,
			OWLOntologyManager m) {
		OWLOntology o;
		try {
			o = m.loadOntologyFromOntologyDocument(IRI.create(props.getPhysicalURI()));
		} catch (OWLOntologyCreationException e) {
			if (e.getCause() instanceof FileNotFoundException) {
				LOG.warning("Ontology in location " + props.getPhysicalURI()
						+ " does not exist yet. Setting profile to OWL 2.");
				ctx.setExpressiveness(ContextExpressiveness.OWL2FULL);
			} else {
				LOG.log(Level.SEVERE,
						"Unable to load ontology from location " + props.getPhysicalURI(), e);
			}
			return;
		}
		resolveProfile(ctx, o);
	}

	private static void resolveOwldbProfile(Context ctx, OntologyStorageProperties props,
			OWLOntologyManager m) {
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
			ctx.setExpressiveness(ContextExpressiveness.OWL2FULL);
			return;
		}
		resolveProfile(ctx, o);
		((OWLDBOntology) o).destroyConnection();
	}

	private static void resolveProfile(Context ctx, OWLOntology o) {
		for (OWLProfile p : PROFILES) {
			final OWLProfileReport r = p.checkOntology(o);
			if (r.isInProfile()) {
				ctx.setExpressiveness(PROFILE_NAMES.get(p.getClass()));
				return;
			}
		}
		LOG.warning("The ontology with URI " + ctx.getUri()
				+ " does not correspond to any supported OWL profile.");
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
