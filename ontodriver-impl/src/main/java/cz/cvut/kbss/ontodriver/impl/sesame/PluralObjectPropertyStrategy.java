package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.vocabulary.RDF;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.exceptions.OWLReferencedListException;
import cz.cvut.kbss.ontodriver.exceptions.OWLSimpleListException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Strategy for plural object property attributes. </p>
 * 
 * I. e. collections of references to other entities.
 * 
 * @author ledvima1
 * 
 */
class PluralObjectPropertyStrategy extends AttributeStrategy {

	protected PluralObjectPropertyStrategy(SesameModuleInternal internal, SubjectModels models) {
		super(internal, models);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws OntoDriverException, IllegalArgumentException, IllegalAccessException {
		if (!alwaysLoad && att.getFetchType() == FetchType.LAZY) {
			return;
		}
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
	<T> void save(URI primaryKey, Attribute<?, ?> att, Object value, URI context, boolean removeOld)
			throws OntoDriverException {
		assert (att instanceof PluralAttribute<?, ?, ?>);
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		final URI attUri = getAddressAsSesameUri(att.getIRI());
		switch (pa.getCollectionType()) {
		case SET:
			final Set<?> set = Set.class.cast(value);
			saveSet(primaryKey, attUri, set, context, removeOld);
			break;
		case LIST:
			final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
			final List<?> lst = List.class.cast(value);
			switch (la.getSequenceType()) {
			case referenced:
				saveReferencedList(primaryKey, attUri, la, lst, context, removeOld);
				break;
			case simple:
				saveSimpleList(primaryKey, attUri, la, lst, context, removeOld);
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
		final URI hasSequenceUri = getAddressAsSesameUri(la.getIRI());
		final boolean includeInferred = la.isInferred();
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final URI hasContentsUri = getAddressAsSesameUri(la.getOWLPropertyHasContentsIRI());

		Value seq = getPropertyValue(subject, hasSequenceUri, includeInferred);
		if (!isUri(seq)) {
			throw new OWLReferencedListException("The value of property " + hasSequenceUri
					+ " has to be an URI.");
		}
		List<Object> lst = new ArrayList<>();
		URI seqUri = (URI) seq;
		while (seq != null) {
			URI content = getObjectPropertyValue(seqUri, hasContentsUri, includeInferred, false);
			if (content == null) {
				break;
			}
			Object inst = getJavaInstanceForSubject(cls, content);
			assert inst != null;
			lst.add(inst);
			seq = getPropertyValue(seqUri, hasNextUri, includeInferred);
			if (!isUri(seq)) {
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
		final URI property = getAddressAsSesameUri(pa.getIRI());

		final Class<?> cls = pa.getBindableJavaType();
		final Collection<Statement> statements = filter(subject, property, null, pa.isInferred());
		if (statements.isEmpty()) {
			return null;
		}
		final Set<Object> set = new HashSet<>(statements.size());
		for (Statement stmt : statements) {
			final Value obj = stmt.getObject();
			if (!isUri(obj)) {
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
		final URI property = getAddressAsSesameUri(la.getIRI());
		final boolean includeInferred = la.isInferred();
		// Element type
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());

		final List<Object> lst = new ArrayList<>();
		final Value val = getPropertyValue(subject, property, includeInferred);
		if (val == null) {
			return null;
		}
		if (!isUri(val)) {
			throw new OWLSimpleListException("The value of property " + property
					+ " has to be an URI.");
		}
		URI newSubject = (URI) val;
		while (newSubject != null) {
			final Object o = getJavaInstanceForSubject(cls, newSubject);
			lst.add(o);
			final Value nextValue = getPropertyValue(newSubject, hasNextUri, includeInferred);
			if (nextValue == null) {
				break;
			}
			if (!isUri(nextValue)) {
				throw new OWLSimpleListException("The value of property " + hasNextUri
						+ " has to be an URI.");
			}
			newSubject = (URI) nextValue;
		}
		return lst;
	}

	private void saveSet(URI primaryKey, URI propertyUri, Set<?> values, URI context,
			boolean removeOld) throws OntoDriverException {
		if (removeOld) {
			removeOldObjectPropertyValues(primaryKey, propertyUri, context);
		}
		if (values == null || values.isEmpty()) {
			return;
		}
		final List<Statement> stmts = new ArrayList<>(values.size());
		for (Object val : values) {
			final URI id = getIdentifier(val);
			assert id != null;
			final Statement stmt = valueFactory.createStatement(id, propertyUri, primaryKey);
			stmts.add(stmt);
		}
		addIndividualsForReferencedEntities(values, context);
		addStatements(stmts, context);
	}

	private void saveSimpleList(URI primaryKey, URI hasSequence, ListAttribute<?, ?> la,
			List<?> values, URI context, boolean removeOld) throws OntoDriverException {
		if (removeOld) {
			removeOldList(primaryKey, hasSequence,
					getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI()), la.isInferred(),
					context);
		}
		if (values == null || values.isEmpty()) {
			return;
		}
		final List<Statement> toSave = new ArrayList<>(values.size());
		final URI hasNext = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final Iterator<?> it = values.iterator();
		assert it.hasNext();
		Object val = it.next();
		URI seq = getIdentifier(val);
		toSave.add(valueFactory.createStatement(primaryKey, hasSequence, seq));
		while (it.hasNext()) {
			URI next = getIdentifier(it.next());
			toSave.add(valueFactory.createStatement(seq, hasNext, next));
			seq = next;
		}
		addIndividualsForReferencedEntities(values, context);
		addStatements(toSave, context);
	}

	private void removeOldList(URI primaryKey, URI hasSequence, URI hasNext,
			boolean includeInferred, URI context) {
		final List<Statement> toRemove = new LinkedList<>();
		final Set<URI> ctxs = Collections.singleton(context);
		final Value val = getPropertyValue(primaryKey, hasSequence, includeInferred, ctxs);
		if (val == null) {
			return;
		}
		if (!isUri(val)) {
			throw new OWLSimpleListException("The value of property " + hasSequence
					+ " has to be an URI.");
		}
		URI seq = (URI) val;
		toRemove.add(valueFactory.createStatement(primaryKey, hasSequence, seq));
		while (seq != null) {
			final Value next = getPropertyValue(seq, hasNext, includeInferred, ctxs);
			if (next == null) {
				break;
			}
			if (!isUri(next)) {
				throw new OWLSimpleListException("The value of property " + hasNext
						+ " has to be an URI.");
			}
			removeTemporaryIndividual((URI) next);
			toRemove.add(valueFactory.createStatement(seq, hasNext, next));
			seq = (URI) next;
		}
		removeStatements(toRemove, context);
	}

	private void saveReferencedList(URI primaryKey, URI hasSequence, ListAttribute<?, ?> la,
			List<?> values, URI context, boolean removeOld) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Setting referenced list " + primaryKey + ", sequence=" + values);
		}
		if (removeOld) {
			removeOldList(primaryKey, hasSequence,
					getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI()), la.isInferred(),
					context);
		}
		final int size = values == null ? 3 : (values.size() * 2 + 3);
		final List<Statement> toSave = new ArrayList<>(size);
		final String localName = primaryKey.getLocalName(); // ~
															// IRI.getFragment()
		final URI listUri = generatePrimaryKey(localName + "-SEQ");
		toSave.add(valueFactory.createStatement(listUri, RDF.TYPE,
				getAddressAsSesameUri(la.getOWLListClass())));
		toSave.add(valueFactory.createStatement(primaryKey, hasSequence, listUri));
		if (values == null || values.isEmpty()) {
			addStatements(toSave, context);
			return;
		}

		final URI hasContents = getAddressAsSesameUri(la.getOWLPropertyHasContentsIRI());
		final URI hasNext = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final Iterator<?> it = values.iterator();
		assert it.hasNext();
		Object val = it.next();
		toSave.add(valueFactory.createStatement(listUri, hasContents, getIdentifier(val)));

		int i = 1;
		URI next = listUri;
		while (it.hasNext()) {
			val = it.next();
			final URI next2 = generatePrimaryKey(localName + "-SEQ" + (i++));
			toSave.add(valueFactory.createStatement(next, hasNext, next2));
			toSave.add(valueFactory.createStatement(next2, hasContents, getIdentifier(val)));
			next = next2;
		}
		addIndividualsForReferencedEntities(values, context);
		addStatements(toSave, context);
	}
}
