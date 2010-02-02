package cz.cvut.kbss.owlpersistence.owlapi.old;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.inference.OWLReasoner;
import org.semanticweb.owlapi.inference.OWLReasonerException;
import org.semanticweb.owlapi.inference.OWLReasonerFactory;
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

import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.simpleversion.model.owlapi.OWLAPIv3OWL2Ontology;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.owlapi.AbstractEntityManagerImpl;
import cz.cvut.kbss.owlpersistence.owlapi.EntityManagerFactoryImpl;

public class EntityManagerImplOld extends AbstractEntityManagerImpl {

	public EntityManagerImplOld(EntityManagerFactoryImpl emf,
			Map<String, String> map) {
		super(emf, map);
	}

	private OWLReasonerFactory f;
	private OWLReasoner r;

	@Override
	protected OWLNamedIndividual asOWLNamedIndividual(OWLIndividual i) {
		return i.asNamedIndividual();
	}

	@Override
	protected OWLTypedLiteral asOWLTypedLiteral(OWLLiteral l) {
		return l.asOWLStringLiteral();
	}

	@Override
	protected boolean containsIndividualInSignature(IRI i) {
		return o.containsIndividualReference(i);
	}

	@Override
	protected void createReasoner() {
		r = f.createReasoner(m, Collections.singleton(merged));
	}

	@Override
	protected Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual i,
			OWLDataProperty e) {
		try {
			return r.getRelatedValues(i, e);
		} catch (OWLReasonerException e1) {
			throw new OWLPersistenceException(e1);
		}
	}

	@Override
	protected Set<OWLNamedIndividual> getInstances(OWLClassExpression ce,
			boolean direct) {
		try {
			return r.getIndividuals(ce, direct);
		} catch (OWLReasonerException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	protected Set<OWLNamedIndividual> getObjectPropertyValues(
			OWLNamedIndividual i, OWLObjectProperty e) {
		try {
			return r.getRelatedIndividuals(i, e);
		} catch (OWLReasonerException e1) {
			throw new OWLPersistenceException(e1);
		}
	}

	@Override
	protected OWL2Ontology<OWLObject> getQueryOntology() {
		return new OWLAPIv3OWL2Ontology(m, merged, r);
	}

	@Override
	protected boolean isOWLTypedLiteral(OWLLiteral l) {
		return l.isTyped();
	}

	@Override
	protected OWLOntology loadOntologyFromOntologyDocument(URI uri) {
		try {
			return m.loadOntologyFromPhysicalURI(uri);
		} catch (OWLOntologyCreationException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	protected OWLOntologyIRIMapper setupMappings(final Map<URI, URI> map) {
		return new OWLOntologyIRIMapper() {

			@Override
			public URI getPhysicalURI(IRI arg0) {
				return map.get(arg0.toURI());
			}
		};
	}

	@Override
	protected Object transform(OWLLiteral c) {
		return DatatypeTransformer.transform(c);
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
		try {
			r.clearOntologies();
			r.loadOntologies(m.getOntologies());
			r.realise();
		} catch (OWLReasonerException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	protected Set<OWLClass> getTypes(OWLNamedIndividual i, boolean direct) {
		final Set<OWLClass> set = new HashSet<OWLClass>();

		try {
			for (final Set<OWLClass> s : r.getTypes(i, direct)) {
				set.addAll(s);
			}
		} catch (OWLReasonerException e) {
			e.printStackTrace();
		}

		return set;
	}
}
