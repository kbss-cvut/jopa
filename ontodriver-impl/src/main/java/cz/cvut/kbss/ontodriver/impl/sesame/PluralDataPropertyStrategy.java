package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;

/**
 * Strategy for plural data property attributes. </p>
 * 
 * I. e. collections of non-entity types saved as data properties.
 * 
 * @author ledvima1
 * 
 */
class PluralDataPropertyStrategy extends AttributeStrategy {

	protected PluralDataPropertyStrategy(SesameModuleInternal internal, SubjectModels models) {
		super(internal, models);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad) {
		throw new NotYetImplementedException("Collection data properties are not implemented yet.");
	}

	@Override
	<T> void save(URI primaryKey, Attribute<?, ?> att, Object value, URI context, boolean removeOld) {
		assert att instanceof PluralAttribute<?, ?, ?>;
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		final URI attUri = getAddressAsSesameUri(pa.getIRI());
		switch (pa.getCollectionType()) {
		case SET:
			saveDataPropertyValues(primaryKey, attUri, value, context, removeOld);
			break;
		case LIST:
		case MAP:
		case COLLECTION:
			throw new NotYetImplementedException();
		}
	}

	private void saveDataPropertyValues(URI primaryKey, URI propertyUri, Object value, URI context,
			boolean removeOld) {
		if (removeOld) {
			removeOldDataPropertyValues(primaryKey, propertyUri, context);
		}
		final Set<?> set = Set.class.cast(value);
		final List<Statement> stmts = new ArrayList<>(set.size());
		for (Object ob : set) {
			Statement stmt = valueFactory.createStatement(primaryKey, propertyUri,
					SesameUtils.createDataPropertyLiteral(ob, lang, valueFactory));
			stmts.add(stmt);
		}
		addStatements(stmts, context);
	}
}
