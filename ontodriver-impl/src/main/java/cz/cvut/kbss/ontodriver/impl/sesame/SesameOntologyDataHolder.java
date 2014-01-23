package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.ValueFactory;

/**
 * Holder for information about a Sesame ontology.
 * 
 * @author ledvima1
 * 
 */
final class SesameOntologyDataHolder {

	private final StorageProxy storage;
	private final ValueFactory valueFactory;
	private final String language;

	SesameOntologyDataHolder(SesameDataHolderBuilder builder) {
		assert builder.storage != null : "argument storage is null";
		assert builder.vf != null : "argument valueFactory is null";
		this.storage = builder.storage;
		this.valueFactory = builder.vf;
		this.language = builder.lang;
	}

	public StorageProxy getStorage() {
		return storage;
	}

	ValueFactory getValueFactory() {
		return valueFactory;
	}

	String getLanguage() {
		return language;
	}

	static SesameDataHolderBuilder storage(StorageProxy st) {
		return new SesameDataHolderBuilder().storage(st);
	}

	static SesameDataHolderBuilder valueFactory(ValueFactory vf) {
		return new SesameDataHolderBuilder().valueFactory(vf);
	}

	static SesameDataHolderBuilder language(String lang) {
		return new SesameDataHolderBuilder().language(lang);
	}

	static final class SesameDataHolderBuilder {
		private StorageProxy storage;
		private ValueFactory vf;
		private String lang;

		private SesameDataHolderBuilder() {
		}

		SesameDataHolderBuilder storage(StorageProxy st) {
			this.storage = st;
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
