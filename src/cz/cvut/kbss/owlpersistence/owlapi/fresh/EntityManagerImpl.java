package cz.cvut.kbss.owlpersistence.owlapi.fresh;

import java.io.File;
import java.net.URI;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLTypedLiteral;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.simpleversion.model.owlapi.OWLAPIv3OWL2OntologyNew;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.owlapi.AbstractEntityManagerImpl;
import cz.cvut.kbss.owlpersistence.owlapi.EntityManagerFactoryImpl;

public class EntityManagerImpl extends AbstractEntityManagerImpl {

	private OWLReasoner r;
	private OWLReasonerFactory f;

	public EntityManagerImpl(EntityManagerFactoryImpl emf,
			Map<String, String> map) {
		super(emf, map);
	}

	@Override
	protected OWLNamedIndividual asOWLNamedIndividual(OWLIndividual i) {
		return i.asOWLNamedIndividual();
	}

	@Override
	protected OWLTypedLiteral asOWLTypedLiteral(OWLLiteral l) {
		return l.asOWLTypedLiteral();
	}

	@Override
	protected boolean containsIndividualInSignature(IRI i) {
		return o.containsIndividualInSignature(i);
	}

	@Override
	protected Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual i,
			OWLDataProperty e) {
		r.flush();
		return r.getDataPropertyValues(i, e);
	}

	@Override
	protected Set<OWLNamedIndividual> getObjectPropertyValues(
			OWLNamedIndividual i, OWLObjectProperty e) {
		r.flush();
		return r.getObjectPropertyValues(i, e).getFlattened();
	}

	@Override
	protected OWL2Ontology<OWLObject> getQueryOntology() {
		return new OWLAPIv3OWL2OntologyNew(m, merged, r);
	}

	@Override
	protected boolean isOWLTypedLiteral(OWLLiteral l) {
		return l.isOWLTypedLiteral();
	}

	@Override
	protected OWLOntology loadOntologyFromOntologyDocument(URI file) {
		try {
			return m.loadOntologyFromOntologyDocument(new File(file));
		} catch (OWLOntologyCreationException e) {
			throw new OWLPersistenceException();
		}
	}

	@Override
	protected Object transform(OWLLiteral c) {
		return DatatypeTransformer.transform(c);
	}

	@Override
	protected void createReasoner() {
		r = f.createReasoner(o);
		r.prepareReasoner();
	}

	@Override
	protected Set<OWLNamedIndividual> getInstances(OWLClassExpression ce,
			boolean direct) {
		r.flush();
		return r.getInstances(ce, direct).getFlattened();
	}

	@Override
	protected OWLOntologyIRIMapper setupMappings(final Map<URI, URI> map) {
		return new OWLOntologyIRIMapper() {

			@Override
			public IRI getDocumentIRI(IRI arg0) {
				if (!map.containsKey(arg0.toURI())) {
					return arg0;
				}
				
				return IRI.create(map.get(arg0.toURI()));
			}
		};
	}

	@Override
	protected void setupReasonerFactory(String rfClass) {
		try {
			this.f = (OWLReasonerFactory) Class.forName(rfClass).newInstance();
		} catch (InstantiationException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		} catch (ClassNotFoundException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	protected void reinitReasoner() {
		// nothing TODO
//		createReasoner();
	}

	@Override
	protected Set<OWLClass> getTypes(OWLNamedIndividual i, boolean direct) {
		r.flush();
		return r.getTypes(i,direct).getFlattened();
	}
}
