package cz.cvut.kbss.owlpersistence.owlapi.identityreasoner;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

public class OWLAPIIdentityReasonerFactory implements OWLReasonerFactory {

	@Override
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
		return createNonBufferingReasoner(ontology, null);
	}

	@Override
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology,
			OWLReasonerConfiguration config) {
		return new OWLAPIIdentityReasoner(ontology);
	}

	@Override
	public OWLReasoner createReasoner(OWLOntology ontology) {
		return createReasoner(ontology, null);
	}

	@Override
	public OWLReasoner createReasoner(OWLOntology ontology,
			OWLReasonerConfiguration config) {
		return new OWLAPIIdentityReasoner(ontology);
	}

	@Override
	public String getReasonerName() {
		return "Identity reasoner";
	}
}
