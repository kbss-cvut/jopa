package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class TypesHandler {

	private static final Logger LOG = Logger.getLogger(TypesHandler.class.getName());

	private final SesameModuleInternal internal;
	private ValueFactory factory;
	private StorageProxy storage;

	TypesHandler(SesameModuleInternal internal) {
		this.internal = internal;
		this.factory = internal.getValueFactory();
		this.storage = internal.getStorage();
	}

	<T> void load(T entity, URI entityUri, TypesSpecification<?, ?> types, EntityType<T> et,
			Set<URI> contexts) throws IllegalArgumentException, IllegalAccessException {
		final Model m = storage.filter(entityUri, RDF.TYPE, null, types.isInferred(), contexts);
		loadImpl(entity, types, et, m);
	}

	<T> void load(T entity, TypesSpecification<?, ?> types, EntityType<T> entityType,
			SubjectModels statements) throws IllegalArgumentException, IllegalAccessException {
		final Model m = types.isInferred() ? statements.getInferredModel() : statements
				.getAssertedModel();
		final Model res = m.filter(null, RDF.TYPE, null,
				SesameUtils.varargs(statements.getSesameContexts()));
		loadImpl(entity, types, entityType, res);
	}

	/**
	 * Saves type statements (with predicate rdf:type) about the specified
	 * entity into the ontology. </p>
	 * 
	 * This includes removing type statements which are no longer relevant and
	 * adding new type assertions.
	 * 
	 * @param entity
	 *            The entity
	 * @param uri
	 *            Entity primary key
	 * @param types
	 *            TypesSpecification
	 * @param entityType
	 *            Entity type as resolved from the metamodel
	 * @param context
	 *            URI of context into which the types information will be saved
	 * @param deleteOld
	 *            Whether to remove old types information. E. g. for newly
	 *            persisted entities this will be false because there is
	 *            obviously no type information to remove yet
	 * @throws OntoDriverException
	 *             If the types are inferred
	 */
	<T> void save(T entity, URI uri, TypesSpecification<?, ?> types, EntityType<T> entityType,
			URI context, boolean deleteOld) throws OntoDriverException, IllegalArgumentException,
			IllegalAccessException {
		if (types.isInferred()) {
			throw new OntoDriverException("Inferred fields cannot be set externally.");
		}
		URI typeUri = factory.createURI(entityType.getIRI().toString());
		Object value = types.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving types of " + entity + " with value = " + value);
		}
		if (value != null && !(value instanceof Set)) {
			throw new IllegalArgumentException("The types attribute has to be a java.util.Set.");
		}

		final Set<?> set = value != null ? (Set<?>) value : Collections.emptySet();
		final Set<Statement> toAdd = new HashSet<>(set.size());
		for (Object type : set) {
			toAdd.add(factory.createStatement(uri, RDF.TYPE, factory.createURI(type.toString())));
		}
		// Delete the old types
		if (deleteOld) {
			final Set<Statement> currentTypes = storage.filter(uri, RDF.TYPE, null, false,
					Collections.singleton(context));
			final Set<Statement> toRemove = new HashSet<>(currentTypes.size());
			for (Statement stmt : currentTypes) {
				final Value val = stmt.getObject();
				assert internal.isUri(val);
				if (val.equals(typeUri)) {
					continue;
				}
				if (!toAdd.remove(stmt)) {
					toRemove.add(stmt);
				}
			}
			internal.removeStatements(toRemove, context);
		}
		internal.addStatements(toAdd, context);
	}

	private <T> void loadImpl(T entity, TypesSpecification<?, ?> types, EntityType<T> et,
			Model statements) throws IllegalArgumentException, IllegalAccessException {
		final Set<Object> res = new HashSet<>();
		final String typeIri = et.getIRI().toString();
		for (Statement stmt : statements) {
			final String tp = stmt.getObject().stringValue();
			if (tp.equals(typeIri)) {
				continue;
			}
			res.add(tp);
		}

		types.getJavaField().set(entity, res);
	}

}
