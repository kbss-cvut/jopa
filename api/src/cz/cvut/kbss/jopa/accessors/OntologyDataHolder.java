package cz.cvut.kbss.jopa.accessors;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

public final class OntologyDataHolder {

	private final OWLOntology workingOntology;
	private final OWLOntology reasoningOntology;
	private final OWLOntologyManager ontologyManager;
	private final OWLDataFactory dataFactory;
	private final OWLReasoner reasoner;

	private final Metamodel metamodel;

	OntologyDataHolder(DataHolderBuilder builder) {
		this.workingOntology = builder.workingOntology;
		this.reasoningOntology = builder.reasoningOntology;
		this.ontologyManager = builder.ontologyManager;
		this.dataFactory = builder.dataFactory;
		this.reasoner = builder.reasoner;
		this.metamodel = builder.metamodel;
	}

	OWLOntology getWorkingOntology() {
		return workingOntology;
	}

	OWLOntology getReasoningOntology() {
		return reasoningOntology;
	}

	OWLOntologyManager getOntologyManager() {
		return ontologyManager;
	}

	OWLDataFactory getDataFactory() {
		return dataFactory;
	}

	OWLReasoner getReasoner() {
		return reasoner;
	}

	Metamodel getMetamodel() {
		return metamodel;
	}

	public static class DataHolderBuilder {
		private OWLOntology workingOntology;
		private OWLOntology reasoningOntology;
		private OWLOntologyManager ontologyManager;
		private OWLDataFactory dataFactory;
		private OWLReasoner reasoner;

		private Metamodel metamodel;

		DataHolderBuilder() {
		}

		DataHolderBuilder workingOntology(OWLOntology ontology) {
			this.workingOntology = ontology;
			return this;
		}

		DataHolderBuilder reasoningOntology(OWLOntology ontology) {
			this.reasoningOntology = ontology;
			return this;
		}

		DataHolderBuilder ontologyManager(OWLOntologyManager manager) {
			this.ontologyManager = manager;
			return this;
		}

		DataHolderBuilder dataFactory(OWLDataFactory dataFactory) {
			this.dataFactory = dataFactory;
			return this;
		}

		DataHolderBuilder reasoner(OWLReasoner reasoner) {
			this.reasoner = reasoner;
			return this;
		}

		DataHolderBuilder metamodel(Metamodel metamodel) {
			this.metamodel = metamodel;
			return this;
		}

		OntologyDataHolder build() {
			return new OntologyDataHolder(this);
		}
	}
}
