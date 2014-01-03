package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.exceptions.OWLReferencedListException;
import cz.cvut.kbss.ontodriver.exceptions.OWLSimpleListException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class PluralObjectPropertyStrategy extends AttributeStrategy {

	public PluralObjectPropertyStrategy(SesameModuleInternal internal) {
		super(internal);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws OntoDriverException, IllegalArgumentException, IllegalAccessException {
		assert (att instanceof PluralAttribute<?, ?, ?>);
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		switch (pa.getCollectionType()) {
		case LIST:
			final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
			List<?> lst = null;
			switch (la.getSequenceType()) {
			case referenced:
				lst = loadReferencedList(uri, la);
				break;
			case simple:
				lst = loadSimpleList(uri, la);
				break;
			}
			pa.getJavaField().set(entity, lst);
			break;
		case SET:
			Set<?> set = loadReferencedSet(uri, pa);
			pa.getJavaField().set(entity, set);
			break;
		case COLLECTION:
		case MAP:
			throw new NotYetImplementedException("NOT YET IMPLEMENTED");
		}
	}

	@Override
	<T> void save(T entity, URI uri, Attribute<?, ?> att, URI attUri, Object value)
			throws OntoDriverException {
		assert (att instanceof PluralAttribute<?, ?, ?>);
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		switch (pa.getCollectionType()) {
		case SET:
			final Set<?> set = Set.class.cast(value);
			saveSet(uri, attUri, set);
			break;
		case LIST:
			final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
			final List<?> lst = List.class.cast(value);
			switch (la.getSequenceType()) {
			case referenced:
				saveReferencedList(uri, attUri, la, lst);
				break;
			case simple:
				saveSimpleList(uri, attUri, la, lst);
				break;
			}
			break;
		case MAP:
		case COLLECTION:
			throw new NotYetImplementedException();
		}
	}

	/**
	 * Loads referenced list specified by the ListAttribute.
	 * 
	 * @param subject
	 *            Subject (entity) URI
	 * @param la
	 *            attribute
	 * @return list of entities or null if there are none
	 * @throws OntoDriverException
	 */
	private List<?> loadReferencedList(URI subject, ListAttribute<?, ?> la)
			throws OntoDriverException {
		final URI hasSequenceUri = internal.getAddressAsSesameUri(la.getIRI());
		final boolean includeInferred = la.isInferred();
		final Model m = internal.getModel(includeInferred);
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = internal.getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final URI hasContentsUri = internal
				.getAddressAsSesameUri(la.getOWLPropertyHasContentsIRI());

		Value seq = getPropertyValue(subject, hasSequenceUri, m);
		if (!internal.isUri(seq)) {
			throw new OWLReferencedListException("The value of property " + hasSequenceUri
					+ " has to be an URI.");
		}
		List<Object> lst = new ArrayList<>();
		URI seqUri = (URI) seq;
		while (seq != null) {
			URI content = getObjectPropertyValue(seqUri, hasContentsUri, includeInferred);
			if (content == null) {
				break;
			}
			Object inst = getJavaInstanceForSubject(cls, content);
			assert inst != null;
			lst.add(inst);
			seq = getPropertyValue(seqUri, hasNextUri, m);
			if (!internal.isUri(seq)) {
				throw new OWLReferencedListException("The value of property " + hasNextUri
						+ " has to be an URI.");
			}
			seqUri = (URI) seq;
		}
		if (lst.isEmpty()) {
			lst = null;
		}
		return lst;
	}

	/**
	 * Loads set of object property references.
	 * 
	 * @param subject
	 *            Subject (entity) uri
	 * @param pa
	 *            Attribute to load
	 * @return The loaded set or null if no references were found
	 */
	private Set<?> loadReferencedSet(URI subject, PluralAttribute<?, ?, ?> pa)
			throws OntoDriverException {
		final URI property = internal.getAddressAsSesameUri(pa.getIRI());
		final Model m = internal.getModel(pa.isInferred());

		final Class<?> cls = pa.getBindableJavaType();
		final Collection<Statement> statements = m.filter(subject, property, null);
		if (statements.isEmpty()) {
			return null;
		}
		final Set<Object> set = new HashSet<>(statements.size());
		for (Statement stmt : statements) {
			final Value obj = stmt.getObject();
			if (!internal.isUri(obj)) {
				continue;
			}
			final URI objUri = (URI) obj;
			final Object o = getJavaInstanceForSubject(cls, objUri);
			set.add(o);
		}
		return set;
	}

	/**
	 * Loads simple list specified by the ListAttribute.
	 * 
	 * @param subject
	 *            Subject (entity) URI
	 * @param la
	 *            attribute
	 * @return list of entities or null if there are none
	 */
	private List<?> loadSimpleList(URI subject, ListAttribute<?, ?> la) throws OntoDriverException {
		final URI property = internal.getAddressAsSesameUri(la.getIRI());
		final Model m = internal.getModel(la.isInferred());
		// Element type
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = internal.getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());

		final List<Object> lst = new ArrayList<>();
		final Value val = getPropertyValue(subject, property, m);
		if (val == null) {
			return null;
		}
		if (!internal.isUri(val)) {
			throw new OWLSimpleListException("The value of property " + property
					+ " has to be an URI.");
		}
		URI newSubject = (URI) val;
		while (newSubject != null) {
			final Object o = getJavaInstanceForSubject(cls, newSubject);
			lst.add(o);
			final Value nextValue = getPropertyValue(newSubject, hasNextUri, m);
			if (nextValue == null) {
				break;
			}
			if (!internal.isUri(nextValue)) {
				throw new OWLSimpleListException("The value of property " + hasNextUri
						+ " has to be an URI.");
			}
			newSubject = (URI) nextValue;
		}
		return lst;
	}

	private void saveSet(URI uri, URI propertyUri, Set<?> values) throws OntoDriverException {
		removeOldObjectPropertyValues(uri, propertyUri);
		final List<Statement> stmts = new ArrayList<>(values.size());
		for (Object val : values) {
			final URI id = internal.getIdentifier(val);
			assert id != null;
			final Statement stmt = valueFactory.createStatement(id, propertyUri, uri);
			stmts.add(stmt);
		}
		internal.addIndividualsForReferencedEntities(values);
		internal.addStatements(stmts);
	}

	private void saveSimpleList(URI uri, URI propertyUri, ListAttribute<?, ?> la, List<?> values) {
		removeOldList(uri, propertyUri,
				internal.getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI()),
				la.isInferred());
		if (values == null || values.isEmpty()) {
			return;
		}
		final List<Statement> toSave = new ArrayList<>(values.size());
		final URI hasNext = internal.getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final Iterator<?> it = values.iterator();
		assert it.hasNext();
		Object val = it.next();
		URI seq = internal.getIdentifier(val);
		toSave.add(valueFactory.createStatement(uri, propertyUri, seq));
		while (it.hasNext()) {
			URI next = internal.getIdentifier(it.next());
			toSave.add(valueFactory.createStatement(seq, hasNext, next));
			seq = next;
		}
		internal.addStatements(toSave);
	}

	private void removeOldList(URI uri, URI hasSequence, URI hasNext, boolean includeInferred) {
		final List<Statement> toRemove = new LinkedList<>();
		final Model m = internal.getModel(includeInferred);
		final Value val = getPropertyValue(uri, hasSequence, m);
		if (val == null) {
			return;
		}
		if (!internal.isUri(val)) {
			throw new OWLSimpleListException("The value of property " + hasSequence
					+ " has to be an URI.");
		}
		URI seq = (URI) val;
		toRemove.add(valueFactory.createStatement(uri, hasSequence, seq));
		while (seq != null) {
			final Value next = getPropertyValue(seq, hasNext, m);
			if (next == null) {
				break;
			}
			if (!internal.isUri(next)) {
				throw new OWLSimpleListException("The value of property " + hasNext
						+ " has to be an URI.");
			}
			internal.removeTemporaryIndividual((URI) next);
			toRemove.add(valueFactory.createStatement(seq, hasNext, next));
			seq = (URI) next;
		}
		internal.removeStatements(toRemove);
	}

	private void saveReferencedList(URI uri, URI propertyUri, ListAttribute<?, ?> la, List<?> values) {
		// TODO
	}
}
