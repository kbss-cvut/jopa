package cz.cvut.kbss.ontodriver.impl.utils;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

public final class OwlapiConnectorDataHolder {

	private final OWLOntology workingOntology;
	private final OWLOntology reasoningOntology;
	private final OWLOntologyManager ontologyManager;
	private final OWLDataFactory dataFactory;
	private final OWLReasoner reasoner;

	OwlapiConnectorDataHolder(DataHolderBuilder builder) {
		this.workingOntology = builder.workingOntology;
		this.reasoningOntology = builder.reasoningOntology;
		this.ontologyManager = builder.ontologyManager;
		this.dataFactory = builder.dataFactory;
		this.reasoner = builder.reasoner;
	}

	public OWLOntology getWorkingOntology() {
		return workingOntology;
	}

	public OWLOntology getReasoningOntology() {
		return reasoningOntology;
	}

	public OWLOntologyManager getOntologyManager() {
		return ontologyManager;
	}

	public OWLDataFactory getDataFactory() {
		return dataFactory;
	}

	public OWLReasoner getReasoner() {
		return reasoner;
	}

	public static DataHolderBuilder workingOntology(OWLOntology ontology) {
		return new DataHolderBuilder().workingOntology(ontology);
	}

	public static DataHolderBuilder reasoningOntology(OWLOntology ontology) {
		return new DataHolderBuilder().reasoningOntology(ontology);
	}

	public static DataHolderBuilder ontologyManager(OWLOntologyManager manager) {
		return new DataHolderBuilder().ontologyManager(manager);
	}

	public static DataHolderBuilder dataFactory(OWLDataFactory factory) {
		return new DataHolderBuilder().dataFactory(factory);
	}

	public static DataHolderBuilder reasoner(OWLReasoner reasoner) {
		return new DataHolderBuilder().reasoner(reasoner);
	}

	public static class DataHolderBuilder {
		private OWLOntology workingOntology;
		private OWLOntology reasoningOntology;
		private OWLOntologyManager ontologyManager;
		private OWLDataFactory dataFactory;
		private OWLReasoner reasoner;

		DataHolderBuilder() {
		}

		public DataHolderBuilder workingOntology(OWLOntology ontology) {
			this.workingOntology = ontology;
			return this;
		}

		public DataHolderBuilder reasoningOntology(OWLOntology ontology) {
			this.reasoningOntology = ontology;
			return this;
		}

		public DataHolderBuilder ontologyManager(OWLOntologyManager manager) {
			this.ontologyManager = manager;
			return this;
		}

		public DataHolderBuilder dataFactory(OWLDataFactory dataFactory) {
			this.dataFactory = dataFactory;
			return this;
		}

		public DataHolderBuilder reasoner(OWLReasoner reasoner) {
			this.reasoner = reasoner;
			return this;
		}

		public OwlapiConnectorDataHolder build() {
			return new OwlapiConnectorDataHolder(this);
		}
	}
}
