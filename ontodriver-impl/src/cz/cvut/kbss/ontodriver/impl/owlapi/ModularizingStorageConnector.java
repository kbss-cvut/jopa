package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import uk.ac.manchester.cs.owlapi.modularity.ModuleType;
import uk.ac.manchester.cs.owlapi.modularity.SyntacticLocalityModuleExtractor;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * The modularizing storage connector uses syntactic locality based modules to
 * work with the ontology. </p>
 * 
 * It maintains only one copy of the whole ontology, the {@code StorageModule}s
 * are provided with an extracted modules of the ontology which entail only the
 * information relevant to their metamodel. </p>
 * 
 * This class implements the <i>Decorator</i> pattern, it contains an
 * {@code OwlapiStorageConnector} and decorates its functionality with the
 * module extractor.
 * 
 * @author kidney
 * @param <X>
 * 
 */
public class ModularizingStorageConnector implements StorageConnector {

	private static final Logger LOG = Logger
			.getLogger(ModularizingStorageConnector.class.getName());

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final OwlapiStorageConnector connector;

	private boolean open;

	public ModularizingStorageConnector(OwlapiStorageConnector connector)
			throws OntoDriverException {
		if (connector == null) {
			throw new NullPointerException();
		}
		this.connector = connector;
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing modularizing storage connector.");
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	public void saveWorkingOntology() throws OntoDriverException {
		WRITE.lock();
		try {
			connector.saveWorkingOntology();
		} finally {
			WRITE.unlock();
		}
	}

	/**
	 * Applies the specified changes to the shared underlying ontology. </p>
	 * 
	 * @param changes
	 *            The changes to apply
	 * @throws OntoDriverException
	 *             If change application fails
	 * @throws NullPointerException
	 *             If {@code changes} is {@code null}
	 */
	public void applyChanges(List<OWLOntologyChange> changes) throws OntoDriverException {
		if (changes == null) {
			throw new NullPointerException();
		}
		if (changes.isEmpty()) {
			return;
		}
		WRITE.lock();
		try {
			connector.applyChanges(changes);
		} finally {
			WRITE.unlock();
		}
	}

	/**
	 * Retrieves number of class assertions in the shared ontology. </p>
	 * 
	 * This is useful for automatic primary key generation.
	 * 
	 * @return Number of class assertions
	 */
	public int getClassAssertionsCount() {
		READ.lock();
		try {
			return connector.getClassAssertionsCount();
		} finally {
			READ.unlock();
		}
	}

	/**
	 * Extracts ontology module entailing data for the entity metamodel. </p>
	 * 
	 * The module is returned as a new ontology with IRI corresponding to the
	 * specified ontology context.
	 * 
	 * @param metamodel
	 *            {@code Metamodel}
	 * @param ctx
	 *            Ontology context
	 * @return Data holder with the extracted ontology
	 * @throws OntoDriverException
	 *             If the extraction fails
	 * @throws NullPointerException
	 *             If {@code metamodel} or {@code context} is {@code null}
	 */
	public OwlapiConnectorDataHolder extractOntologyModule(Metamodel metamodel, Context ctx)
			throws OntoDriverException {
		if (metamodel == null || ctx == null) {
			throw new NullPointerException();
		}
		Set<OWLEntity> signature = null;
		READ.lock();
		try {
			signature = getSignatureForExtraction(metamodel);
			return extractModuleInternal(signature, ctx);
		} finally {
			READ.unlock();
		}
	}

	/**
	 * Extracts a syntactic locality based module for the specified signature.
	 * </p>
	 * 
	 * The module is used as an ontology with URI corresponding to the specified
	 * context.
	 * 
	 * @param signature
	 *            Signature to use
	 * @param ctx
	 *            Context
	 * @return {@code OwlapiConnectorDataHolder}
	 * @throws OntoDriverException
	 *             If the module extraction fails
	 */
	private OwlapiConnectorDataHolder extractModuleInternal(Set<OWLEntity> signature, Context ctx)
			throws OntoDriverException {
		// Extract a syntactic locality-based module
		try {
			SyntacticLocalityModuleExtractor sme = new SyntacticLocalityModuleExtractor(
					connector.ontologyManager, connector.workingOntology, ModuleType.STAR);
			Set<OWLAxiom> mod = sme.extract(signature);
			if (LOG.isLoggable(Level.FINEST)) {
				LOG.finest("Extracted module size: " + mod.size());
			}
			final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
			final OWLDataFactory d = m.getOWLDataFactory();
			final OWLOntology o = m.createOntology(mod, IRI.create(ctx.getUri()));
			OWLReasoner r = connector.getReasonerFactory().createReasoner(o);
			final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder.ontologyManager(m)
					.workingOntology(o).reasoningOntology(o).dataFactory(d).reasoner(r)
					.language(connector.language).build();
			return holder;
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException("Unable to extract ontology module.", e);
		}
	}

	/**
	 * Generates signature for entities and their fields from the the specified
	 * metamodel. </p>
	 * 
	 * TODO: types are not in the extracted module, this method doesn't create
	 * full signature of the metamodel
	 * 
	 * @param metamodel
	 *            {@code Metamodel}
	 * @return Set of {@code OWLEntity} representing the signature seed
	 */
	private Set<OWLEntity> getSignatureForExtraction(Metamodel metamodel) {
		assert metamodel != null;
		final Set<EntityType<?>> entities = metamodel.getEntities();
		// Just guess the size
		final Set<OWLEntity> signature = new HashSet<OWLEntity>(entities.size() * 5);
		for (EntityType<?> t : entities) {
			// Take the class
			final OWLClass cls = connector.dataFactory.getOWLClass(IRI
					.create(t.getIRI().toString()));
			signature.add(cls);
			final Set<?> atts = t.getAttributes();
			// And all its attributes
			for (Object o : atts) {
				final Attribute<?, ?> att = (Attribute<?, ?>) o;
				final Field attField = att.getJavaField();
				OWLEntity ent = null;
				final IRI attIri = IRI.create(att.getIRI().toString());
				if (attField.isAnnotationPresent(OWLObjectProperty.class)) {
					ent = connector.dataFactory.getOWLObjectProperty(attIri);
				} else if (attField.isAnnotationPresent(OWLDataProperty.class)) {
					ent = connector.dataFactory.getOWLDataProperty(attIri);
				} else if (attField.isAnnotationPresent(OWLAnnotationProperty.class)) {
					ent = connector.dataFactory.getOWLAnnotationProperty(attIri);
				}
				// TODO Is this all?
				signature.add(ent);
			}
		}
		return signature;
	}
}
