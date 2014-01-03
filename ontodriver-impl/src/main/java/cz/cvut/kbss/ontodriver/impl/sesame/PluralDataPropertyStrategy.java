package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;

class PluralDataPropertyStrategy extends AttributeStrategy {

	public PluralDataPropertyStrategy(SesameModuleInternal internal) {
		super(internal);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad) {
		throw new NotYetImplementedException("Collection data properties are not implemented yet.");
	}

	@Override
	<T> void save(T entity, URI uri, Attribute<?, ?> att, URI attUri, Object value) {
		assert att instanceof PluralAttribute<?, ?, ?>;
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		switch (pa.getCollectionType()) {
		case SET:
			saveDataPropertyValues(uri, attUri, value);
			break;
		case LIST:
		case MAP:
		case COLLECTION:
			throw new NotYetImplementedException();
		}
	}

	private void saveDataPropertyValues(URI uri, URI propertyUri, Object value) {
		removeOldDataPropertyValues(uri, propertyUri);
		final Set<?> set = Set.class.cast(value);
		final List<Statement> stmts = new ArrayList<>(set.size());
		for (Object ob : set) {
			Statement stmt = valueFactory.createStatement(uri, propertyUri,
					SesameUtils.createDataPropertyLiteral(ob, lang, valueFactory));
			stmts.add(stmt);
		}
		internal.addStatements(stmts);
	}
}
