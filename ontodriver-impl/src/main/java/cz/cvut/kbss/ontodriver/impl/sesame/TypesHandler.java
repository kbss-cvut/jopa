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

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
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

	<T> void load(T entity, URI entityUri, EntityType<T> et, Descriptor descriptor)
			throws IllegalArgumentException, IllegalAccessException {
		final Descriptor typesDescriptor = descriptor.getAttributeDescriptor(et.getTypes());
		final URI context = SesameUtils.toSesameUri(typesDescriptor.getContext(), factory);
		final Model m = storage.filter(entityUri, RDF.TYPE, null, et.getTypes().isInferred(),
				context);
		loadImpl(entity, et, m);
	}

	<T> void load(T entity, EntityType<T> entityType, SubjectModels<T> statements)
			throws IllegalArgumentException, IllegalAccessException {
		final Model res = statements.filter(null, RDF.TYPE, null, entityType.getTypes()
				.isInferred(), statements.getFieldContext(entityType.getTypes()));
		loadImpl(entity, entityType, res);
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
	 * @param entityType
	 *            Entity type as resolved from the metamodel
	 * @param descriptor
	 *            Entity descriptor, contains info about context
	 * @param deleteOld
	 *            Whether to remove old types information. E. g. for newly
	 *            persisted entities this will be false because there is
	 *            obviously no type information to remove yet
	 * @throws OntoDriverException
	 *             If the types are inferred
	 */
	<T> void save(T entity, URI uri, EntityType<T> entityType, Descriptor descriptor,
			boolean deleteOld) throws OntoDriverException, IllegalArgumentException,
			IllegalAccessException {
		final TypesSpecification<?, ?> types = entityType.getTypes();
		if (types.isInferred()) {
			throw new OWLInferredAttributeModifiedException("Cannot modify attribute " + types
					+ ", it is inferred.");
		}
		final Descriptor typesDescriptor = descriptor.getAttributeDescriptor(entityType.getTypes());
		final URI context = SesameUtils.toSesameUri(typesDescriptor.getContext(), factory);
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
			final Set<Statement> currentTypes = storage.filter(uri, RDF.TYPE, null, false, context);
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

	private <T> void loadImpl(T entity, EntityType<T> et, Model statements)
			throws IllegalArgumentException, IllegalAccessException {
		final Set<Object> res = new HashSet<>();
		final String typeIri = et.getIRI().toString();
		for (Statement stmt : statements) {
			final String tp = stmt.getObject().stringValue();
			if (tp.equals(typeIri)) {
				continue;
			}
			res.add(tp);
		}
		et.getTypes().getJavaField().set(entity, res);
	}

}
