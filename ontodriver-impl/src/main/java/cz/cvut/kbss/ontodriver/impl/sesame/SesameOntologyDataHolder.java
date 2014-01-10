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
	private final String language;

	SesameOntologyDataHolder(SesameDataHolderBuilder builder) {
		assert builder.explicitModel != null : "argument explicitModel is null";
		assert builder.vf != null : "argument valueFactory is null";
		this.model = builder.model;
		this.explicitModel = builder.explicitModel;
		this.valueFactory = builder.vf;
		this.language = builder.lang;
	}

	Model getModel() {
		return model;
	}

	ValueFactory getValueFactory() {
		return valueFactory;
	}

	Model getExplicitModel() {
		return explicitModel;
	}

	String getLanguage() {
		return language;
	}

	static SesameDataHolderBuilder model(Model model) {
		return new SesameDataHolderBuilder().model(model);
	}

	static SesameDataHolderBuilder explicitModel(Model explicitModel) {
		return new SesameDataHolderBuilder().explicitModel(explicitModel);
	}

	static SesameDataHolderBuilder valueFactory(ValueFactory vf) {
		return new SesameDataHolderBuilder().valueFactory(vf);
	}

	static SesameDataHolderBuilder language(String lang) {
		return new SesameDataHolderBuilder().language(lang);
	}

	static final class SesameDataHolderBuilder {
		private Model model;
		private Model explicitModel;
		private ValueFactory vf;
		private String lang;

		private SesameDataHolderBuilder() {
		}

		SesameDataHolderBuilder model(Model model) {
			this.model = model;
			return this;
		}

		SesameDataHolderBuilder explicitModel(Model explModel) {
			this.explicitModel = explModel;
			return this;
		}

		SesameDataHolderBuilder valueFactory(ValueFactory valueFactory) {
			this.vf = valueFactory;
			return this;
		}

		SesameDataHolderBuilder language(String language) {
			this.lang = language;
			return this;
		}

		SesameOntologyDataHolder build() {
			return new SesameOntologyDataHolder(this);
		}
	}
}
