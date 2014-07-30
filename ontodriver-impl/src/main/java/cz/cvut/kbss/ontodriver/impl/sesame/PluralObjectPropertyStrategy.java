package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.Collection;
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
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
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

	protected PluralObjectPropertyStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		super(internal, models);
	}

	@Override
	<T> void load(Attribute<?, ?> att, boolean alwaysLoad) throws OntoDriverException,
			IllegalArgumentException, IllegalAccessException {
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
				lst = loadReferencedList(la);
				break;
			case simple:
				lst = loadSimpleList(la);
				break;
			}
			pa.getJavaField().set(models.entity, lst);
			break;
		case SET:
			Set<?> set = loadReferencedSet(pa);
			pa.getJavaField().set(models.entity, set);
			break;
		case COLLECTION:
		case MAP:
			throw new NotYetImplementedException("NOT YET IMPLEMENTED");
		}
	}

	@Override
	<T> void save(Attribute<?, ?> att, Object value, boolean removeOld) throws OntoDriverException {
		assert (att instanceof PluralAttribute<?, ?, ?>);
		final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) att;
		switch (pa.getCollectionType()) {
		case SET:
			final Set<?> set = Set.class.cast(value);
			saveSet(att, set, removeOld);
			break;
		case LIST:
			final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
			final List<?> lst = List.class.cast(value);
			switch (la.getSequenceType()) {
			case referenced:
				saveReferencedList(la, lst, removeOld);
				break;
			case simple:
				saveSimpleList(la, lst, removeOld);
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
	private List<?> loadReferencedList(ListAttribute<?, ?> la) throws OntoDriverException {
		final URI hasSequenceUri = getAddressAsSesameUri(la.getIRI());
		final boolean includeInferred = la.isInferred();
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final URI hasContentsUri = getAddressAsSesameUri(la.getOWLPropertyHasContentsIRI());
		final URI ctx = models.getFieldContext(la);

		Value seq = getPropertyValue(models.primaryKey, hasSequenceUri, includeInferred, ctx);
		if (!isUri(seq)) {
			throw new OWLReferencedListException("The value of property " + hasSequenceUri
					+ " has to be an URI.");
		}
		List<Object> lst = new ArrayList<>();
		URI seqUri = (URI) seq;
		while (seq != null) {
			URI content = getObjectPropertyValue(seqUri, hasContentsUri, includeInferred, ctx);
			if (content == null) {
				break;
			}
			Object inst = getJavaInstanceForSubject(cls, content, descriptorForContext(ctx));
			assert inst != null;
			lst.add(inst);
			seq = getPropertyValue(seqUri, hasNextUri, includeInferred, ctx);
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

	private Descriptor descriptorForContext(URI context) {
		return new EntityDescriptor(context != null ? java.net.URI.create(context.stringValue())
				: null);
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
	private Set<?> loadReferencedSet(PluralAttribute<?, ?, ?> pa) throws OntoDriverException {
		final URI property = getAddressAsSesameUri(pa.getIRI());
		final URI ctx = models.getFieldContext(pa);

		final Class<?> cls = pa.getBindableJavaType();
		final Collection<Statement> statements = filter(models.primaryKey, property, null,
				pa.isInferred(), ctx);
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
			final Object o = getJavaInstanceForSubject(cls, objUri, descriptorForContext(ctx));
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
	private List<?> loadSimpleList(ListAttribute<?, ?> la) throws OntoDriverException {
		final URI property = getAddressAsSesameUri(la.getIRI());
		final boolean includeInferred = la.isInferred();
		// Element type
		final Class<?> cls = la.getBindableJavaType();
		final URI hasNextUri = getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI());
		final URI ctx = models.getFieldContext(la);

		final List<Object> lst = new ArrayList<>();
		final Value val = getPropertyValue(models.primaryKey, property, includeInferred, ctx);
		if (val == null) {
			return null;
		}
		if (!isUri(val)) {
			throw new OWLSimpleListException("The value of property " + property
					+ " has to be an URI.");
		}
		URI newSubject = (URI) val;
		while (newSubject != null) {
			final Object o = getJavaInstanceForSubject(cls, newSubject, descriptorForContext(ctx));
			lst.add(o);
			final Value nextValue = getPropertyValue(newSubject, hasNextUri, includeInferred, ctx);
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

	/**
	 * Saves the specified set of values.
	 * 
	 * @param att
	 *            Attribute descriptor
	 * @param values
	 *            The values to save
	 * @param removeOld
	 *            Whether to remove old values of the attribute
	 * @throws OntoDriverException
	 */
	private void saveSet(Attribute<?, ?> att, Set<?> values, boolean removeOld)
			throws OntoDriverException {
		final URI propertyUri = getAddressAsSesameUri(att.getIRI());
		final URI context = models.getFieldContext(att);
		if (removeOld) {
			removeOldObjectPropertyValues(models.primaryKey, propertyUri, context);
		}
		if (values == null || values.isEmpty()) {
			return;
		}
		final List<Statement> stmts = new ArrayList<>(values.size());
		for (Object val : values) {
			final URI id = getIdentifier(val);
			assert id != null;
			final Statement stmt = valueFactory.createStatement(models.primaryKey, propertyUri, id);
			stmts.add(stmt);
		}
		addIndividualsForReferencedEntities(values, context);
		addStatements(stmts, context);
	}

	/**
	 * Saves the specified simple list of values.
	 * 
	 * @param la
	 *            Attribute descriptor
	 * @param values
	 *            The values to save
	 * @param removeOld
	 *            Whether to remove old values of the attribute
	 * @throws OntoDriverException
	 */
	private void saveSimpleList(ListAttribute<?, ?> la, List<?> values, boolean removeOld)
			throws OntoDriverException {
		final URI hasSequence = getAddressAsSesameUri(la.getIRI());
		final URI context = models.getFieldContext(la);
		if (removeOld) {
			removeOldList(models.primaryKey, hasSequence,
					getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI()), context);
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
		toSave.add(valueFactory.createStatement(models.primaryKey, hasSequence, seq));
		while (it.hasNext()) {
			URI next = getIdentifier(it.next());
			toSave.add(valueFactory.createStatement(seq, hasNext, next));
			seq = next;
		}
		addIndividualsForReferencedEntities(values, context);
		addStatements(toSave, context);
	}

	/**
	 * Removes old values of a simple or referenced list.
	 * 
	 * @param primaryKey
	 *            Subject primary key
	 * @param hasSequence
	 *            Sequence URI
	 * @param hasNext
	 *            Next element URI
	 * @param includeInferred
	 *            Whether to include inferred
	 * @param context
	 *            Context of the attribute values
	 */
	private void removeOldList(URI primaryKey, URI hasSequence, URI hasNext, URI context) {
		final List<Statement> toRemove = new LinkedList<>();
		final Value val = getPropertyValue(models.primaryKey, hasSequence, false, context);
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
			final Value next = getPropertyValue(seq, hasNext, false, context);
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

	/**
	 * Saves values of the specified referenced list.
	 * 
	 * @param la
	 *            Attribute descriptor
	 * @param values
	 *            The values to save
	 * @param removeOld
	 *            Whether to remove old values of the attribute
	 * @throws OntoDriverException
	 */
	private void saveReferencedList(ListAttribute<?, ?> la, List<?> values, boolean removeOld)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Setting referenced list " + models.primaryKey + ", sequence=" + values);
		}
		final URI hasSequence = getAddressAsSesameUri(la.getIRI());
		final URI context = models.getFieldContext(la);
		if (removeOld) {
			removeOldList(models.primaryKey, hasSequence,
					getAddressAsSesameUri(la.getOWLObjectPropertyHasNextIRI()), context);
		}
		final int size = values == null ? 3 : (values.size() * 2 + 3);
		final List<Statement> toSave = new ArrayList<>(size);
		final String localName = models.primaryKey.getLocalName(); // ~
		// IRI.getFragment()
		final URI listUri = generatePrimaryKey(localName + "-SEQ");
		toSave.add(valueFactory.createStatement(listUri, RDF.TYPE,
				getAddressAsSesameUri(la.getOWLListClass())));
		toSave.add(valueFactory.createStatement(models.primaryKey, hasSequence, listUri));
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
