package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.Model;
import org.openrdf.model.ValueFactory;

/**
 * Holder for information about a Sesame ontology.
 * 
 * @author ledvima1
 * 
 */
final class SesameOntologyDataHolder {

	// Model containing explicit and inferred statements
	private final Model model;
	// Model containing only explicit statements
	private final Model explicitModel;
	private final ValueFactory valueFactory;

	SesameOntologyDataHolder(Model model, Model explicitModel, ValueFactory valueFactory) {
		assert explicitModel != null : "argument explicitModel is null";
		assert valueFactory != null : "argument valueFactory is null";
		this.model = model;
		this.explicitModel = explicitModel;
		this.valueFactory = valueFactory;
	}

	Model getModel() {
		return model;
	}

	ValueFactory getValueFactory() {
		return valueFactory;
	}

	public Model getExplicitModel() {
		return explicitModel;
	}
}
