/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.lang.reflect.Field;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationValueVisitor;
import org.semanticweb.owlapi.model.OWLAnonymousIndividual;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.model.RemoveImport;
import org.semanticweb.owlapi.model.RemoveOntologyAnnotation;
import org.semanticweb.owlapi.model.SetOntologyID;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.OWLEntityRemover;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3QueryFactory;
import cz.cvut.kbss.owlpersistence.model.EntityTransaction;
import cz.cvut.kbss.owlpersistence.model.IntegrityConstraintViolatedException;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ListAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.TypesSpecification;
import cz.cvut.kbss.owlpersistence.model.query.Query;
import cz.cvut.kbss.owlpersistence.model.query.TypedQuery;
import cz.cvut.kbss.owlpersistence.util.MappingFileParser;

public class EntityManagerImpl extends AbstractEntityManager {

	private static final Logger LOG = Logger.getLogger(EntityManagerImpl.class
			.getName());
	private OWLReasonerFactory rf;
	private OWLReasoner r;
	private OWLDataFactory f;
	private OWLOntology workingOnt;
	private OWLOntology reasoningOnt;
	private OWLOntologyManager m;
	private EntityManagerFactoryImpl emf;
	/**
	 * A collection of all entities that are currently managed by the
	 * persistence context. This collection includes also newly added entities
	 * that are not persistent yet.
	 */
	private final Map<Object, OWLNamedIndividual> managed = new HashMap<Object, OWLNamedIndividual>();
	/**
	 * A collection of all entities that are currently removed from the
	 * persistence context.
	 */
	private final Map<Object, OWLNamedIndividual> removed = new HashMap<Object, OWLNamedIndividual>();
	private final Map<Object, List<OWLOntologyChange>> removeChanges = Collections
			.synchronizedMap(new HashMap<Object, List<OWLOntologyChange>>());
	private final List<OWLOntologyChange> allChanges = Collections
			.synchronizedList(new ArrayList<OWLOntologyChange>());
	private boolean open;
	private String lang = "en";
	private EntityTransaction tx = null;

	public EntityManagerImpl(EntityManagerFactoryImpl emf,
			Map<String, String> map) {
		this.emf = emf;

		final String ontologyURI = map
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = map
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String reasonerFactoryClass = map
				.get(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS);
		final String dbConnection = map
				.get(OWLAPIPersistenceProperties.ONTOLOGY_DB_CONNECTION);
		final String languageTag = map.get(OWLAPIPersistenceProperties.LANG);

		if (languageTag != null) {
			lang = languageTag;
		}

		try {
			this.rf = (OWLReasonerFactory) Class.forName(reasonerFactoryClass)
					.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException("Error instantiating factory '"
					+ reasonerFactoryClass + "'.", e);
		}

		if (ontologyURI == null) {
			throw new IllegalArgumentException(
					"Either a document URL or an ontology URI must be specified.");
		}

		if (LOG.isLoggable(Level.INFO)) {
			LOG.info("Loading model ontologyURI='" + ontologyURI
					+ "', mappingFileURI='" + mappingFileURI + "'.");
		}
		try {

			if (dbConnection != null) {
				LOG.info("Using database backend: " + dbConnection);
				// TODO
				// this.m = Class.forName(OWLDBManagerclassName)
				// .createOWLOntologyManager(OWLDataFactoryImpl
				// .getInstance());

			} else {
				this.m = OWLManager.createOWLOntologyManager();
			}

			this.f = m.getOWLDataFactory();

			final Map<URI, URI> mapping = getMappings(mappingFileURI);
			LOG.info("Found mappings = " + mapping);

			m.addIRIMapper(new OWLOntologyIRIMapper() {
				public IRI getDocumentIRI(IRI arg0) {
					if (!mapping.containsKey(arg0.toURI())) {
						return arg0;
					}

					return IRI.create(mapping.get(arg0.toURI()));
				}
			});
			LOG.info("Mapping file succesfully parsed.");

			if (dbConnection != null) {
				// workingOnt = m.loadOntology(IRI.create(dbConnection));
				// LOG.info("INDS: "
				// + workingOnt.getIndividualsInSignature().size());
				// m.saveOntology(workingOnt);
			} else {
				URI physicalURI = mapping.get(URI.create(ontologyURI));

				if (physicalURI == null) {
					physicalURI = URI.create(ontologyURI);
				}

				if (physicalURI != null) {
					workingOnt = m.loadOntologyFromOntologyDocument(new File(
							physicalURI));
				} else if (ontologyURI.startsWith("file:")) {
					workingOnt = m.loadOntologyFromOntologyDocument(new File(
							URI.create(ontologyURI)));
				} else {
					workingOnt = m.loadOntology(IRI.create(ontologyURI));
				}
			}
			reasoningOnt = new OWLOntologyMerger(m).createMergedOntology(m,
					IRI.create("http://temporary"));
			LOG.info("Ontology " + ontologyURI + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
		}
		try {
			r = rf.createReasoner(reasoningOnt);
			r.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}

		this.open = true;
	}

	private enum State {
		NEW, MANAGED, DETACHED, REMOVED;
	}

	/**
	 * This method takes an OWLClass instance and saves it to the persistence
	 * context.
	 * 
	 * @throws OWLPersistenceException
	 *             whenever the entity is already persisted.
	 */
	public void persist(final Object entity) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Persisting " + entity);
		}
		ensureOpen();

		switch (getState(entity)) {
		case NEW:
			try {
				final Class<?> cls = entity.getClass();
				final EntityType<?> e = getMetamodel().entity(cls);

				IRI id = getIdentifier(entity);

				if (id == null && e.getIdentifier().isGenerated()) {
					id = createNewID(entity.getClass().getSimpleName());
					setIdentifier(entity, id);
				}

				if (id == null) {
					throw new OWLPersistenceException(
							"The id for entity "
									+ entity
									+ " is null and it is not specified as 'generated' ");
				}

				if ((workingOnt.containsIndividualInSignature(id, true) || (managed
						.values().contains(f.getOWLNamedIndividual(id))))
						&& !entity.getClass().isEnum()) {
					throw new OWLPersistenceException("An entity with URI "
							+ id + " is already persisted within the context.");
				}

				managed.put(entity, f.getOWLNamedIndividual(id));

				final OWLNamedIndividual ii = managed.get(entity);

				final OWLClassAssertionAxiom aa = f.getOWLClassAssertionAxiom(
						f.getOWLClass(IRI.create(e.getIRI().toString())), ii);

				addChange(new AddAxiom(workingOnt, aa));

				final TypesSpecification<?, ?> types = e.getTypes();
				if (types != null) {
					_saveTypesReference(entity, types);
				}

				final PropertiesSpecification<?, ?> properties = e
						.getProperties();
				if (properties != null) {
					_savePropertiesReference(entity, properties);
				}

				for (final Attribute<?, ?> a : e.getAttributes()) {
					_saveReference(entity, a);
				}

			} catch (Exception e) {
				throw new OWLPersistenceException(
						"A problem occured when persisting " + entity, e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					try {
						Object ox = at.getJavaField().get(o);
						System.out.println("object=" + o + ", attribute="
								+ at.getName() + ", value=" + ox);

						if (ox == null) {
							return;
						}

						if (at.isCollection()) {
							for (final Object ox2 : (Collection<?>) ox) {
								persist(ox2);
							}
						} else {
							persist(ox);
						}
					} catch (Exception e) {
						throw new OWLPersistenceException(
								"A problem occured when persisting attribute "
										+ at.getName() + " of with value " + o
										+ " of object " + entity, e);
					}
				}
			}.start(this, entity, CascadeType.PERSIST);
			break;
		case DETACHED:
			final Object id1 = getEntityManagerFactory()
					.getPersistenceUnitUtil().getIdentifier(entity);
			for (final Object o : managed.keySet()) {
				if (id1 == getEntityManagerFactory().getPersistenceUnitUtil()
						.getIdentifier(o)) {
					throw new IllegalArgumentException();
				}
			}
			break;
		case REMOVED:
			managed.put(entity, removed.remove(entity));
			allChanges.removeAll(removeChanges.remove(entity));
			break;
		}
	}

	public <T> T merge(final T entity) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Merging " + entity);
		}
		ensureOpen();

		Class<T> clz = (Class<T>) entity.getClass();

		switch (getState(entity)) {
		case NEW:
			try {
				final T merged = clz.newInstance();
				new OneLevelCascadeExplorer() {
					@Override
					protected void exploreCascaded(Attribute<?, ?> at, Object o)
							throws IllegalAccessException {
						at.getJavaField().set(merged, merge(o));
					}

					@Override
					protected void exploreNonCascaded(Attribute<?, ?> at,
							Object o) throws IllegalAccessException {
						at.getJavaField().set(merged, at.getJavaField().get(o));
					}
				};
			} catch (InstantiationException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(entity, merge(o));
				}
			}.start(this, entity, CascadeType.MERGE);
			return entity;
		case DETACHED:
			final T merged;

			final OWLNamedIndividual ind = f
					.getOWLNamedIndividual(getIdentifier(entity));

			if (!managed.values().contains(ind)) {
				merged = create(clz, ind);
			} else {
				T x = null;
				for (Object o : managed.keySet()) {
					if (managed.get(o).equals(ind)) {
						x = clz.cast(o);
						break;
					}
				}
				merged = x;
			}

			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, merge(o));
				}

				@Override
				protected void exploreNonCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, at.getJavaField().get(o));
				}
			};
		case REMOVED:
		default:
			throw new IllegalArgumentException();
		}
	}

	public void remove(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
			break;
		case DETACHED:
			throw new IllegalArgumentException();
		case MANAGED:
			removed.put(object, managed.remove(object));
			OWLEntityRemover r = new OWLEntityRemover(m,
					Collections.singleton(workingOnt));
			r.visit(removed.get(object));
			allChanges.addAll(r.getChanges());
			removeChanges.put(object, r.getChanges());
		case REMOVED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					remove(o);
				}
			}.start(this, object, CascadeType.REMOVE);
			break;
		}

	}

	public <T> T find(Class<T> t, Object primaryKey) {
		ensureOpen();
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Finding " + t + " with key " + primaryKey);
		}
		final IRI uri = IRI.create(primaryKey.toString());

		OWLNamedIndividual i = f.getOWLNamedIndividual(uri);

		if (managed.containsValue(i)) {
			for (Object o : managed.keySet()) {
				if (managed.get(o).equals(i)) {
					return t.cast(o);
				}
			}
		}

		if (workingOnt.containsIndividualInSignature(uri, true)) {
			return create(t, i);
		}

		return null;
	}

	public void flush() {
		ensureOpen();

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Flushing ...");
		}

		synchronized (allChanges) {
			if (allChanges.isEmpty()) {
				return;
			}

			try {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("CHANGESET: ");
					for (final OWLOntologyChange c : allChanges) {
						LOG.fine("         " + c);
					}
				}

				m.applyChanges(allChanges);

				if (LOG.isLoggable(Level.INFO)) {
					LOG.info("Writing model to "
							+ m.getOntologyDocumentIRI(workingOnt) + " using "
							+ m.getOntologyFormat(workingOnt));
				}

				m.saveOntology(workingOnt);

				allChanges.clear();
				removeChanges.clear();

				if (LOG.isLoggable(Level.INFO)) {
					LOG.info("Model succesfully stored.");
				}
			} catch (OWLOntologyChangeException e) {
				clear();
			} catch (UnknownOWLOntologyException e) {
				throw new OWLPersistenceException(e);
			} catch (OWLOntologyStorageException e) {
				throw new OWLPersistenceException(e);
			}
		}
	}

	public void refresh(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
		case DETACHED:
		case REMOVED:
			throw new IllegalArgumentException();
		case MANAGED:
			// if (toRefresh.contains(object)) {
			loadObjectFromModel(object, true);
			// toRefresh.remove(object);
			// } else {
			// LOG.warning("Object not refreshed : " + object
			// + " because it is up-to-date.");
			// }
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					refresh(o);
				}
			}.start(this, object, CascadeType.REFRESH);
		}
	}

	public void clear() {
		managed.clear();
		removed.clear();
	}

	public void detach(Object entity) {
		ensureOpen();

		switch (getState(entity)) {
		case MANAGED:
			managed.remove(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case REMOVED:
			removed.remove(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case NEW:
		case DETACHED:
			break;
		}
	}

	public boolean contains(Object entity) {
		ensureOpen();
		return managed.containsKey(entity);
	}

	public void close() {
		ensureOpen();
		open = false;
		// flush();
		// m = null;
		// o = null;
		// r = null;
	}

	public boolean isOpen() {
		return open;
	}

	public EntityTransaction getTransaction() {
		return tx;
	}

	public EntityManagerFactoryImpl getEntityManagerFactory() {
		return emf;
	}

	public Metamodel getMetamodel() {
		return emf.getMetamodel();
	}

	/**
	 * Saves the reference. Force.
	 */
	public void saveReference(Object object, Field field) {

		final EntityType<?> et = getMetamodel().entity(object.getClass());

		try {
			if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				// TODO check invalid property bindings

				_savePropertiesReference(object, et.getProperties());
			} else if (et.getTypes() != null
					&& et.getTypes().getJavaField().equals(field)) {
				// TODO check invalid type bindings

				_saveTypesReference(object, et.getTypes());
			} else {
				_saveReference(object, et.getAttribute(field.getName()));
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	public void loadReference(Object object, Field field) {
		flush();

		final EntityType<?> et = getMetamodel().entity(object.getClass());

		try {
			if (et.getTypes() != null
					&& et.getTypes().getJavaField().equals(field)) {
				_loadTypesReference(object, et.getTypes(), true);
			} else if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				_loadPropertiesReference(object, et.getProperties(), true);
			} else {
				_loadReference(object, et.getAttribute(field.getName()), true);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	public boolean isLoaded(final Object object, final String attributeName) {
		// TODO
		return false;
	}

	public Query<?> createQuery(String qlString) {
		return new QueryImpl(qlString, new OWLAPIv3OWL2Ontology(m,
				reasoningOnt, r), false, this);
	}

	public <T> TypedQuery<T> createQuery(String qlString, Class<T> resultClass) {
		return _createTypedQuery(qlString, resultClass, false);
	}

	public Query<List<String>> createNativeQuery(String sparql) {
		return new QueryImpl(sparql, new OWLAPIv3OWL2Ontology(m, reasoningOnt,
				r), true, this);
	}

	public <T> TypedQuery<T> createNativeQuery(String sparql,
			Class<T> resultClass) {
		return _createTypedQuery(sparql, resultClass, true);
	}

	public <T> T unwrap(Class<T> cls) {
		if (cls.equals(this.getClass())) {
			return cls.cast(this);
		} else if (OWLOntologyManager.class.isAssignableFrom(cls)) {
			return cls.cast(m);
		} else if (OWLDataFactory.class.isAssignableFrom(cls)) {
			return cls.cast(m.getOWLDataFactory());
		} else if (OWLOntology.class.isAssignableFrom(cls)) {
			return cls.cast(workingOnt);
		}

		throw new OWLPersistenceException();
	}

	public Object getDelegate() {
		return unwrap(EntityManagerImpl.class);
	}

	public String getLabel(String iri) {
		String label = null;

		for (final OWLAnnotationAssertionAxiom ax : reasoningOnt
				.getAnnotationAssertionAxioms(IRI.create(iri))) {

			OWLAnnotation a = ax.getAnnotation();
			if (a.getProperty().equals(
					f.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL
							.getIRI()))) {
				final LanguageResolverVisitor v = new LanguageResolverVisitor();
				a.getValue().accept(v);

				if (v.getLang().equals(lang)) {
					return v.getValue();
				}

				if (v.getLang().isEmpty() || (v.getLang() == null)
						|| (label == null)) {
					label = v.getValue();
				}
			}
		}

		if (LOG.isLoggable(Level.WARNING)) {
			LOG.warning("No label found for " + iri + ", using IRI itself.");
		}

		return iri;
	}

	class LanguageResolverVisitor implements OWLAnnotationValueVisitor {

		String value = null;
		String lang = null;

		public void visit(OWLAnonymousIndividual arg0) {
			// not supported - silently ignore
		}

		public void visit(IRI arg0) {
			// not supported - silently ignore
		}

		public String getValue() {
			return value;
		}

		public String getLang() {
			return lang;
		}

		public void visit(OWLLiteral sl) {
			value = sl.getLiteral();
			lang = sl.getLang();
		}
	}

	synchronized void addChanges(final Collection<OWLOntologyChange> c) {
		for (final OWLOntologyChange cc : c) {
			if (allChanges.contains(cc)) {
				continue;
			}

			cc.accept(new OWLOntologyChangeVisitor() {

				public void visit(AddAxiom arg0) {
					final RemoveAxiom ax = new RemoveAxiom(workingOnt, arg0
							.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				public void visit(RemoveAxiom arg0) {
					final AddAxiom ax = new AddAxiom(workingOnt, arg0
							.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				public void visit(SetOntologyID arg0) {
					throw new UnsupportedOperationException(
							"Changing ontology URI is not supported.");
				}

				public void visit(AddImport arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology Import is not supported.");
				}

				public void visit(RemoveImport arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology Import is not supported.");
				}

				public void visit(AddOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology annotation is not supported.");
				}

				public void visit(RemoveOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology annotation is not supported.");
				}

			});
		}
	}

	synchronized void addChange(final OWLOntologyChange c) {
		addChanges(Collections.singleton(c));
	}

	private void ensureOpen() {
		if (!isOpen()) {
			throw new OWLPersistenceException("The entity manager is closed !");
		}
	}

	private State getState(Object entity) {
		final Object identifier = getEntityManagerFactory()
				.getPersistenceUnitUtil().getIdentifier(entity);

		if (managed.containsKey(entity)) {
			return State.MANAGED;
		} else if (removed.containsKey(entity)) {
			return State.REMOVED;
		} else if (identifier != null
				&& managed.containsValue(IRI.create(identifier.toString()))) {
			return State.DETACHED;
		} else {
			return State.NEW;
		}
	}

	private void checkCascadeOrPersisted(final CascadeType[] ct,
			final Collection<Object> lst) {
		final boolean cascade = (Arrays.asList(ct).contains(CascadeType.ALL) || Arrays
				.asList(ct).contains(CascadeType.PERSIST));

		if (lst != null) {
			for (final Object li : lst) {
				if (!contains(li)) {
					if (cascade || li.getClass().isEnum()) {
						persist(li);
					} else {
						throw new OWLPersistenceException(
								"The entity is not persisted, neither has cascade type of ALL or PERSIST");
					}
				}
			}
		}
	}

	@Override
	protected void finalize() throws Throwable {
		if (isOpen()) {
			close();
		}
	}

	// private void checkRunningTransaction() {
	// if (tx == null) {
	// throw new OWLPersistenceException(
	// "A transaction context is missing.");
	// }
	// }

	private Map<URI, URI> getMappings(String mappingFileURI) {
		final Map<URI, URI> mapping;
		if (mappingFileURI != null) {
			mapping = MappingFileParser.getMappings(new File(URI
					.create(mappingFileURI)));
		} else {
			mapping = new HashMap<URI, URI>();
		}

		return mapping;
	}

	private <T> T create(final Class<T> realClass, OWLIndividual ii) {
		final Field id = getId(realClass);

		if (ii.isAnonymous()) {
			throw new OWLPersistenceException(
					"Only named individuals are supported: " + ii);
		}

		OWLNamedIndividual i = ii.asOWLNamedIndividual();

		if (id == null) {
			throw new OWLPersistenceException("The id is not defined : "
					+ realClass + " is not a valid OWL persistence class.");
		}

		if (LOG.isLoggable(Level.CONFIG))
			LOG.config("Creating a new instance of " + i);

		T cc = null;
		try {
			cc = (T) realClass.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}

		setIdentifier(cc, i.getIRI());

		managed.put(cc, i);

		loadObjectFromModel(cc, false);

		return cc;
	}

	private IRI createNewID(final String name) {
		// System.out.println("CREATING NEW ID=" + name);
		// System.out.println("    workingOnt=" + workingOnt);
		// System.out.println("    workingOnt.getOntologyID=" +
		// workingOnt.getOntologyID());
		// System.out.println("    workingOnt.getOntologyID.getOntologyIRI=" +
		// workingOnt.getOntologyID());
		final String base = workingOnt.getOntologyID().getOntologyIRI()
				.toString()
				+ "#i_" + name;
		IRI iri = IRI.create(base);

		int i = 1;
		while (workingOnt.containsIndividualInSignature(iri, true)
				|| managed.values().contains(f.getOWLNamedIndividual(iri))) {
			iri = IRI.create(base + "_" + (i++));
		}

		return iri;
	}

	private void setIdentifier(final Object object, final IRI iri) {
		final Field idField = getId(object.getClass());

		try {
			if (iri == null) {
				idField.set(object, null);
			} else if (String.class.equals(idField.getType())) {
				idField.set(object, iri.toString());
			} else if (URI.class.equals(idField.getType())) {
				idField.set(object, iri.toURI());
			} else {
				throw new OWLPersistenceException("Unknown identifier type: "
						+ idField.getType());
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private IRI getIdentifier(final Object object) {
		Object fieldValue = getEntityManagerFactory().getPersistenceUnitUtil()
				.getIdentifier(object);

		if (fieldValue == null) {
			return null;
		}

		try {

			if (fieldValue instanceof String) {
				return IRI.create((String) fieldValue);
			} else if (fieldValue instanceof URI) {
				return IRI.create((URI) fieldValue);
			} else {
				throw new OWLPersistenceException("Unknown identifier type: "
						+ fieldValue.getClass());
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> Field getId(final Class<T> realClass) {
		return emf.getMetamodel().entity(realClass).getIdentifier()
				.getJavaField();
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls,
			final OWLIndividual ii) {
		if (ii.isAnonymous()) {
			throw new OWLPersistenceException(
					"Only named individuals are supported: " + ii);
		}

		OWLNamedIndividual i = ii.asOWLNamedIndividual();

		for (final N object : cls.getEnumConstants()) {
			if (getIdentifier(object).equals(i.getIRI())) {
				return object;
			}
		}

		throw new OWLPersistenceException("Unknown enum constant = " + i);
	}

	/**
	 * TODO
	 * 
	 * An OWLNamedIndividual might be represented by different Java objects to
	 * implement multiple inheritance and polymorphism.
	 * 
	 * However, for each Java class and an identifier, there is at most one
	 * instance in the entitymanager.
	 */
	private <T> T getJavaInstanceForOWLIndividual(final Class<T> cls,
			final OWLIndividual i) {
		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Getting " + i + " of " + cls);
		if (managed.containsValue(i)) {
			for (Object o : managed.keySet()) {
				if (managed.get(o).equals(i)) {
					if (cls.equals(o.getClass())) {
						if (LOG.isLoggable(Level.FINE))
							LOG.fine("Found " + o + ", casting to " + cls);
						return cls.cast(o);
					} else {
						return create(cls, i);
					}
				}
			}
			throw new OWLPersistenceException();
		} else if (cls.isEnum()) {
			return cls.cast(getEnum(cls.asSubclass(Enum.class), i));
		} else {
			return create(cls, i);
		}
	}

	private OWLLiteral javaType2owlLiteral(final Object object) {
		if (object instanceof Integer) {
			return f.getOWLLiteral((Integer) object);
		} else if (object instanceof Boolean) {
			return f.getOWLLiteral((Boolean) object);
		} else if (object instanceof Double) {
			return f.getOWLLiteral((Double) object);
		} else if (object instanceof String) {
			return f.getOWLLiteral((String) object, lang);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
			return f.getOWLLiteral(sdf.format(((Date) object)),
					f.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
		} else {
			throw new IllegalArgumentException();
		}
	}

	private <T> T owlLiteral2javaType(Class<T> t, OWLLiteral c) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Transforming OWLLiteral '" + c + ", to class " + t);
		}
		OWL2Datatype v = OWL2Datatype.XSD_STRING;

		if (!c.isRDFPlainLiteral()) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Datatype : " + c.getDatatype());
			}
			v = c.getDatatype().getBuiltInDatatype();
		}

		Object o = DatatypeTransformer.transform(c);

		if (o == null) {
			throw new OWLPersistenceException("The type is not supported : "
					+ v);
		}

		if (!t.isAssignableFrom(o.getClass())) {
			throw new OWLPersistenceException("The field type '" + t
					+ "' cannot be established from the declared data type '"
					+ v + "'. The declared class is " + o.getClass());
		}

		return t.cast(o);
	}

	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return f.getOWLAnnotationProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLDataProperty dp(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return f.getOWLDataProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLObjectProperty op(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return f.getOWLObjectProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLClass c(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return f.getOWLClass(IRI.create(uri.toString()));
	}

	private void checkIC(Object object, Attribute<?, ?> attribute) {
		LOG.config("CHECKING IC for " + object + ", attribute="
				+ attribute.getIRI());
		try {
			Object value = attribute.getJavaField().get(object);
			Collection<?> set;

			if (value == null) {
				set = Collections.emptySet();
			} else if (attribute.isCollection()) {
				set = (Collection<?>) value;
			} else {
				set = Collections.singleton(value);
			}

			LOG.config("    size=" + set.size());

			for (ParticipationConstraint ic : attribute.getConstraints()) {
				LOG.config("         IC:" + ic.min() + " : " + ic.max());
				if (set.size() < ic.min()
						|| (set.size() > ic.max() && ic.max() >= 0)) {
					throw new IntegrityConstraintViolatedException(
							"Violated min=" + ic.min() + ", max=" + ic.max()
									+ ", for attribute=" + attribute
									+ " of object=" + managed.get(object));
				}
				// TODO FILLER
			}
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void _saveReference(Object object, Attribute<?, ?> attribute)
			throws Exception {
		if (attribute.isInferred()) {
			throw new OWLPersistenceException(
					"Inferred fields must not be set externally.");
		}

		checkIC(object, attribute);

		OWLNamedIndividual subject = managed.get(object);

		Object value = attribute.getJavaField().get(object);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving " + attribute.getName() + " of " + object
					+ " with value = " + value);
		}

		final IRI iri = IRI.create(attribute.getIRI().toString());

		if (attribute.isCollection()) {
			final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) attribute;

			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				final OWLDataProperty dp = f.getOWLDataProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllDataProperties(subject, dp);
					Set set = Set.class.cast(value);
					if (set != null) {
						for (Object element : set) {
							addDataProperty(subject, dp, element);
						}
					}
					break;
				case LIST:
				case COLLECTION:
				case MAP:
					throw new NotYetImplementedException();
				}
				break;
			case OBJECT:
				final OWLObjectProperty op = f.getOWLObjectProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllObjectProperties(subject, op);
					Set set = Set.class.cast(value);
					checkCascadeOrPersisted(attribute.getCascadeTypes(), set);
					if (set != null) {
						for (Object element : set) {
							final OWLNamedIndividual objectValue = m
									.getOWLDataFactory().getOWLNamedIndividual(
											IRI.create((String) getId(clazz)
													.get(element)));

							addObjectProperty(subject, op, objectValue);
						}
					}
					break;
				case LIST:
					final ListAttribute<?, ?> la = (ListAttribute<?, ?>) attribute;
					Class<?> clazz2 = pa.getBindableJavaType();

					final List lst = List.class.cast(value);

					checkCascadeOrPersisted(la.getCascadeTypes(), lst);

					switch (la.getSequenceType()) {
					case referenced:
						setReferencedList(object, clazz2, lst, op,
								c(la.getOWLListClass()),
								op(la.getOWLPropertyHasContentsIRI()),
								op(la.getOWLObjectPropertyHasNextIRI()));
						break;
					case simple:
						setSimpleList(object, clazz2, lst, op,
								op(la.getOWLObjectPropertyHasNextIRI()));
						break;
					}
					break;
				case COLLECTION:
				case MAP:
					throw new NotYetImplementedException();
				}
			}
		} else {
			final SingularAttribute<?, ?> pa = (SingularAttribute<?, ?>) attribute;

			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
				setAnnotationProperty(subject, f.getOWLAnnotationProperty(iri),
						value);
				break;
			case DATA:
				setDataProperty(subject, f.getOWLDataProperty(iri), value);
				break;
			case OBJECT:
				if (value != null) {
					checkCascadeOrPersisted(pa.getCascadeTypes(),
							Collections.singleton(value));
				}

				setObjectPropertyObject(subject, f.getOWLObjectProperty(iri),
						value);
				break;
			}
		}
	}

	private void _saveTypesReference(Object object,
			TypesSpecification<?, ?> spec) throws Exception {
		if (spec.isInferred()) {
			throw new OWLPersistenceException(
					"Inferred fields must not be set externally.");
		}

		OWLNamedIndividual subject = managed.get(object);
		Object value = spec.getJavaField().get(object);

		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving types of " + object + " with value = " + value);
		}

		final EntityType<?> type = emf.getMetamodel().entity(object.getClass());
		final OWLClass myClass = f.getOWLClass(IRI.create(type.getIRI()
				.toString()));

		for (final OWLClassExpression ox : subject.getTypes(workingOnt)) {
			if (ox.equals(myClass) || ox.isAnonymous()) {
				continue;
			}

			addChange(new RemoveAxiom(workingOnt, f.getOWLClassAssertionAxiom(
					ox, subject)));
		}

		Set<String> set = (Set<String>) Set.class.cast(value);
		if (set != null) {
			for (final String x : set) {
				addChange(new AddAxiom(workingOnt, f.getOWLClassAssertionAxiom(
						f.getOWLClass(IRI.create(x)), subject)));
			}
		}
	}

	private void _savePropertiesReference(final Object object,
			final PropertiesSpecification<?, ?> ts)
			throws IllegalAccessException {
		OWLNamedIndividual subject = managed.get(object);

		Object value = ts.getJavaField().get(object);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving other properties of " + object + " with value = "
					+ value);
		}

		final EntityType<?> et = getMetamodel().entity(object.getClass());

		for (final OWLObjectPropertyAssertionAxiom ax : workingOnt
				.getObjectPropertyAssertionAxioms(subject)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}

			boolean found = false;
			for (final Attribute<?, ?> a : et.getAttributes()) {
				if (a.getIRI().equals(
						ax.getProperty().asOWLObjectProperty().getIRI())) {
					found = true;
					break;
				}
			}

			if (found) {
				continue;
			}

			addChange(new RemoveAxiom(workingOnt, m.getOWLDataFactory()
					.getOWLObjectPropertyAssertionAxiom(ax.getProperty(),
							subject, ax.getObject())));

		}

		for (final OWLDataPropertyAssertionAxiom ax : workingOnt
				.getDataPropertyAssertionAxioms(subject)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}

			boolean found = false;
			for (final Attribute<?, ?> a : et.getAttributes()) {
				if (a.getIRI().equals(
						ax.getProperty().asOWLDataProperty().getIRI())) {
					found = true;
					break;
				}
			}

			if (found) {
				continue;
			}

			addChange(new RemoveAxiom(workingOnt, m.getOWLDataFactory()
					.getOWLDataPropertyAssertionAxiom(ax.getProperty(),
							subject, ax.getObject())));

		}

		Map map = Map.class.cast(value);
		if (map != null) {
			for (Object element : map.keySet()) {
				final Object valueSet = map.get(element);

				if (!Set.class.isAssignableFrom(valueSet.getClass())) {
					throw new OWLPersistenceException(
							"EntityManagerImpl : invalid @Properties type, must be Map<String,Set<String>>");
				}

				final IRI propIRI = IRI.create(element + "");

				if (workingOnt.containsDataPropertyInSignature(propIRI)) {
					final OWLDataProperty prop = m.getOWLDataFactory()
							.getOWLDataProperty(IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLLiteral objX = javaType2owlLiteral(ox);

						addChange(new AddAxiom(workingOnt, m
								.getOWLDataFactory()
								.getOWLDataPropertyAssertionAxiom(prop,
										subject, objX)));
					}
				} else {
					// default object property
					final OWLObjectProperty prop = m.getOWLDataFactory()
							.getOWLObjectProperty(IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLNamedIndividual objX = f
								.getOWLNamedIndividual(IRI.create(ox + ""));

						addChange(new AddAxiom(workingOnt, m
								.getOWLDataFactory()
								.getOWLObjectPropertyAssertionAxiom(prop,
										subject, objX)));
					}
				}
			}
		}
	}

	private void loadObjectFromModel(final Object object, boolean force) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetching " + object + " from ontology");
		}
		final Class<?> cls = object.getClass();
		final EntityType<?> type = emf.getMetamodel().entity(cls);

		try {
			final TypesSpecification<?, ?> types = type.getTypes();

			if (types != null) {
				_loadTypesReference(object, types, force);
			}

			final PropertiesSpecification<?, ?> properties = type
					.getProperties();

			if (properties != null) {
				_loadPropertiesReference(object, properties, force);
			}

			for (final Attribute<?, ?> field : type.getAttributes()) {
				_loadReference(object, field, force);
			}
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	// TODO
	private void _loadReference(Object subject, Attribute<?, ?> field,
			boolean all) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Loading " + field + " reference of " + subject);
		}

		try {
			Object value = null;
			final OWLNamedIndividual ii = managed.get(subject);

			switch (field.getPersistentAttributeType()) {
			case ANNOTATION:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of annotations are not supported yet.");
				}
				// TODO value of getAnnotationProperty
				final OWLLiteral laObject = getAnnotationProperty(ii,
						ap(field.getIRI()), field.isInferred());

				if (laObject != null) {
					field.getJavaField().set(subject,
							owlLiteral2javaType(field.getJavaType(), laObject));

				}
				break;
			case DATA:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of data property values are not supported yet.");
				}

				final OWLLiteral lObject = getDataProperty(ii,
						dp(field.getIRI()), field.isInferred());

				if (lObject != null) {
					field.getJavaField().set(subject,
							owlLiteral2javaType(field.getJavaType(), lObject));

				}

				break;
			case OBJECT:
				if (!all && field.getFetchType().equals(FetchType.LAZY)) {
					// toRefresh.add(object);
					break;
				}

				if (field.isCollection()) {
					final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) field;

					switch (pa.getCollectionType()) {
					case LIST:
						final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
						final Class<?> clazz = la.getBindableJavaType();
						switch (la.getSequenceType()) {
						case referenced:
							value = getReferencedList(ii, clazz,
									op(pa.getIRI()), c(la.getOWLListClass()),
									op(la.getOWLPropertyHasContentsIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()),
									field.isInferred());
							break;
						case simple:
							value = getSimpleList(ii, clazz, op(pa.getIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()),
									field.isInferred());
							break;
						}
						break;
					case SET:
						Set<Object> set = new HashSet<Object>();

						for (OWLIndividual col : getObjectProperties(ii,
								op(pa.getIRI()), field.isInferred())) {
							set.add(getJavaInstanceForOWLIndividual(
									pa.getBindableJavaType(), col));
						}

						value = set;
						break;
					case COLLECTION:
					case MAP:
						throw new IllegalArgumentException(
								"NOT YET IMPLEMENTED");
					}
				} else {
					// TODO
					final OWLIndividual iObject = getObjectProperty(ii,
							op(field.getIRI()), field.isInferred());

					if (iObject != null) {
						value = getJavaInstanceForOWLIndividual(
								field.getJavaType(), iObject);
					}
				}

				if (LOG.isLoggable(Level.CONFIG)) {
					LOG.config("Fetched property '" + field.getIRI()
							+ "' into field " + field.getJavaField()
							+ "' of object " + ii + ", value = " + value);
				}
				field.getJavaField().set(subject, value);

				break;
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		} catch (InterruptedException e) {
			throw new OWLPersistenceException(e);
		}
		checkIC(subject, field);
	}

	private void _loadTypesReference(final Object object,
			final TypesSpecification<?, ?> ts, boolean force)
			throws IllegalAccessException {
		Set<Object> set = new HashSet<Object>();
		final OWLNamedIndividual subject = managed.get(object);

		final String iri = getMetamodel().entity(object.getClass()).getIRI()
				.toString();

		if (ts.isInferred()) {
			r.flush();

			for (OWLClass col : r.getTypes(subject, false).getFlattened()) {
				if (iri.equals(col.getIRI().toString())) {
					continue;
				}

				set.add(col.getIRI().toString());
			}

		} else {
			for (OWLClassExpression col : subject.getTypes(workingOnt)) {
				if (col.isAnonymous()
						|| iri.equals(col.asOWLClass().getIRI().toString())) {
					continue;
				}

				set.add(col.asOWLClass().getIRI().toString());
			}

		}

		ts.getJavaField().set(object, set);
	}

	private void _loadPropertiesReference(final Object object,
			final PropertiesSpecification<?, ?> ts, boolean force) {
		final EntityType<?> et = getMetamodel().entity(object.getClass());
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		final OWLNamedIndividual subject = managed.get(object);
		if (ts.isInferred()) {
			r.flush();

			for (final OWLObjectProperty prop : workingOnt
					.getObjectPropertiesInSignature()) {

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (prop.getIRI().toString().equals(a.getIRI().toString())) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = new HashSet<String>();

				for (final OWLNamedIndividual iObject : r
						.getObjectPropertyValues(subject, prop).getFlattened()) {
					set.add(iObject.getIRI().toString());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}

			for (final OWLDataProperty prop : workingOnt
					.getDataPropertiesInSignature()) {
				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (prop.getIRI().toString().equals(a.getIRI().toString())) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = new HashSet<String>();

				for (final OWLLiteral iObject : r.getDataPropertyValues(
						subject, prop)) {
					set.add(iObject.getLiteral());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}
		} else {
			for (final OWLObjectPropertyAssertionAxiom ax : workingOnt
					.getObjectPropertyAssertionAxioms(managed.get(object))) {
				if (ax.getProperty().isAnonymous()) {
					continue;
				}

				final IRI propIRI = ax.getProperty().asOWLObjectProperty()
						.getIRI();

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (a.getIRI().toString().equals(propIRI.toString())) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = map.get(propIRI.toString());
				if (set == null) {
					set = new HashSet<String>();
					map.put(propIRI.toString(), set);
				}

				set.add(ax.getObject().asOWLNamedIndividual().getIRI()
						.toString());
			}

			for (final OWLDataPropertyAssertionAxiom ax : workingOnt
					.getDataPropertyAssertionAxioms(managed.get(object))) {
				if (ax.getProperty().isAnonymous()) {
					continue;
				}

				final IRI propIRI = ax.getProperty().asOWLDataProperty()
						.getIRI();

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (a.getIRI().equals(propIRI)) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = map.get(propIRI.toString());
				if (set == null) {
					set = new HashSet<String>();
					map.put(propIRI.toString(), set);
				}

				set.add(DatatypeTransformer.transform(ax.getObject())
						.toString());
			}
		}
		try {
			ts.getJavaField().set(object, map);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void setObjectPropertyObject(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			Object object) throws InterruptedException {
		OWLNamedIndividual i = null;

		if (object != null) {
			i = f.getOWLNamedIndividual(getIdentifier(object));
		}

		setObjectProperty(src, p, i);
	}

	private void setObjectProperty(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			OWLIndividual i) throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setObjectProperty '" + p + "' of " + src + " to " + i);
		}

		removeAllObjectProperties(src, p);
		if (i != null) {
			addObjectProperty(src, p, i);
		}
	}

	private void setDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty p, Object s)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setDataProperty '" + p + "' of " + i.getIRI() + " to "
					+ s);
		}

		removeAllDataProperties(i, p);
		if (s != null) {
			addDataProperty(i, p, s);
		}
	}

	private void setAnnotationProperty(final OWLNamedIndividual r,
			org.semanticweb.owlapi.model.OWLAnnotationProperty ap,
			Object literalAnnotation) {

		final Collection<OWLOntologyChange> ac = new HashSet<OWLOntologyChange>();

		for (OWLAnnotation annotation : r.getAnnotations(workingOnt, ap)) {
			ac.add(new RemoveAxiom(workingOnt, f
					.getOWLAnnotationAssertionAxiom(r.getIRI(), annotation)));
		}

		if (literalAnnotation != null) {
			ac.add(new AddAxiom(workingOnt, f.getOWLAnnotationAssertionAxiom(r
					.getIRI(), f.getOWLAnnotation(ap,
					javaType2owlLiteral(literalAnnotation)))));
		}

		addChanges(ac);
	}

	private <T> void setReferencedList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE))
			LOG.fine("Setting referenced list " + o + ", sequence=" + sequence);
		removeList(o, hasSequence, hasNext);

		final IRI uri = managed.get(o).getIRI();

		// TODO anonymous

		OWLNamedIndividual seq = f.getOWLNamedIndividual(createNewID(uri
				.getFragment() + "-SEQ"));

		addChange(new AddAxiom(this.workingOnt, f.getOWLClassAssertionAxiom(
				owlList, seq)));

		setObjectProperty(f.getOWLNamedIndividual(uri), hasSequence, seq);

		if (sequence == null || sequence.isEmpty()) {
			return;
		}

		// final OWLNamedIndividual oi = managed.get(sequence
		// .get(sequence.size() - 1));
		//
		// OWLClassExpression d = f.getOWLObjectSomeValuesFrom(hasContents, f
		// .getOWLObjectOneOf(oi));
		//
		// for (int i = sequence.size() - 2; i >= 0; i--) {
		// d = f.getOWLObjectIntersectionOf(f.getOWLObjectSomeValuesFrom(
		// hasNext, d), f.getOWLObjectSomeValuesFrom(hasContents, f
		// .getOWLObjectOneOf(managed.get(sequence.get(i)))));
		// }
		// addChange(new AddAxiom(this.workingOnt,
		// f.getOWLClassAssertionAxiom(d,
		// seq)));

		OWLNamedIndividual ind = managed.get(sequence.get(0));
		addChange(new AddAxiom(this.workingOnt,
				f.getOWLObjectPropertyAssertionAxiom(hasContents, seq, ind)));

		for (int i = 1; i < sequence.size(); i++) {
			OWLNamedIndividual seq2 = f.getOWLNamedIndividual(createNewID(uri
					.getFragment() + "-SEQ" + i));

			addChange(new AddAxiom(this.workingOnt,
					f.getOWLObjectPropertyAssertionAxiom(hasNext, seq, seq2)));

			addChange(new AddAxiom(this.workingOnt,
					f.getOWLObjectPropertyAssertionAxiom(hasContents, seq2,
							managed.get(sequence.get(i)))));

			seq = seq2;
		}
	}

	private <T> void setSimpleList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE))
			LOG.fine("Setting simple list " + o + ", sequence=" + sequence);

		removeList(o, hasSequence, hasNext);

		if (sequence == null) {
			return;
		}

		final Iterator<T> iter = sequence.iterator();

		if (!iter.hasNext()) {
			return;
		}

		OWLNamedIndividual next = managed.get(iter.next());
		setObjectProperty(managed.get(o), hasSequence, next);

		while (iter.hasNext()) {
			final OWLNamedIndividual next2 = managed.get(iter.next());
			setObjectProperty(next, hasNext, next2);
			next = next2;
		}
	}

	private void addObjectProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			final OWLIndividual object) {
		addChange(new AddAxiom(workingOnt, m.getOWLDataFactory()
				.getOWLObjectPropertyAssertionAxiom(property, subject, object)));
	}

	private void addDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			final Object object) {
		addChange(new AddAxiom(workingOnt, f.getOWLDataPropertyAssertionAxiom(
				property, i, javaType2owlLiteral(object))));
	}

	private void removeAllObjectProperties(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws InterruptedException {
		final Collection<? extends OWLIndividual> objects = getObjectProperties(
				subject, property, false);

		if (objects != null) {
			for (final OWLIndividual object : objects) {
				addChange(new RemoveAxiom(workingOnt, m.getOWLDataFactory()
						.getOWLObjectPropertyAssertionAxiom(property, subject,
								object)));
			}
		}
	}

	private void removeAllDataProperties(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property)
			throws InterruptedException {
		final Collection<OWLLiteral> cc = getDataProperties(i, property, false);

		if (cc != null) {
			for (final OWLLiteral s : cc) {
				final OWLAxiom axx = m.getOWLDataFactory()
						.getOWLDataPropertyAssertionAxiom(property, i, s);
				addChange(new RemoveAxiom(workingOnt, axx));
			}
		}
	}

	/**
	 * Removes the list as if it was simple.
	 * 
	 * @param object
	 * @param hasSequence
	 * @param hasNext
	 * @throws OWLReasonerException
	 */
	private void removeList(final Object object,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		OWLIndividual iSequence = getObjectProperty(managed.get(object),
				hasSequence, false);

		// TODO cascading properly

		while (iSequence != null) {
			for (final OWLAxiom a : workingOnt.getReferencingAxioms(iSequence
					.asOWLAnonymousIndividual())) {
				addChange(new RemoveAxiom(workingOnt, a));
			}

			iSequence = getObjectProperty(iSequence, hasNext, false);
		}
	}

	private Collection<? extends OWLIndividual> getObjectProperties(
			final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			boolean inferred) {
		Collection<? extends OWLIndividual> objects;
		if (inferred) {
			r.flush();
			objects = r.getObjectPropertyValues(subject, property)
					.getFlattened();
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getObjectPropertyValues(property, workingOnt);
		}
		return objects;
	}

	private Collection<OWLLiteral> getDataProperties(
			final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			boolean inferred) {
		Collection<OWLLiteral> objects;
		if (inferred) {
			r.flush();
			objects = r.getDataPropertyValues(subject, property);
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getDataPropertyValues(property, workingOnt);
		}
		return objects;
	}

	private OWLIndividual getObjectProperty(final OWLIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLObjectPropertyAssertionAxiom axiom : this.workingOnt
				.getObjectPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLNamedIndividual inferredObject = null;
		if (inferred && subject.isNamed()) {
			r.flush();
			final Set<OWLNamedIndividual> inferredObjects = r
					.getObjectPropertyValues(subject.asOWLNamedIndividual(),
							property).getFlattened();

			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(
							inferredObjects + " should be of size 1, but is "
									+ inferredObjects.size());
				}
			}
		}

		return inferredObject;
	}

	private OWLLiteral getDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLDataPropertyAssertionAxiom axiom : this.workingOnt
				.getDataPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLLiteral inferredObject = null;
		if (inferred) {
			r.flush();
			final Set<OWLLiteral> inferredObjects = r.getDataPropertyValues(
					subject, property);

			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(
							inferredObjects + " should be of size 1, but is "
									+ inferredObjects.size());
				}
			}
		}

		return inferredObject;
	}

	private OWLLiteral getAnnotationProperty(final OWLNamedIndividual i,
			final OWLAnnotationProperty p, boolean inferred) {
		OWLLiteral literal = null;

		for (final OWLOntology o2 : m.getOntologies()) {
			for (final OWLAnnotation a : i.getAnnotations(o2, p)) {
				if (a.getValue() instanceof OWLLiteral) {
					literal = (OWLLiteral) a.getValue();

					if (literal.hasLang(lang)) {
						break;
					}
				}
			}
		}
		LOG.warning("Label requested, but label annotation not found: ");
		return literal;
	}

	private <T> List<T> getReferencedList(final OWLNamedIndividual subject,
			final Class<T> type,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext,
			boolean inferred) throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual seq = getObjectProperty(subject, hasSequence, inferred);

		while (seq != null) {
			OWLIndividual iContent = getObjectProperty(seq, hasContents,
					inferred);

			if (iContent == null) {
				break;
				// TODO
				// throw new
				// IntegrityConstraintViolatedException("No content specified for a list.");
			}

			lst.add(getJavaInstanceForOWLIndividual(type, iContent));
			seq = getObjectProperty(seq, hasNext, inferred);
		}
		return lst;
	}

	private <T> List<T> getSimpleList(final OWLNamedIndividual subject,
			final Class<T> type,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext,
			boolean inferred) throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual o = getObjectProperty(subject, hasSequence, inferred);

		while (o != null) {
			lst.add(getJavaInstanceForOWLIndividual(type, o));
			o = getObjectProperty(o, hasNext, inferred);
		}
		return lst;
	}

	private <T> TypedQuery<T> _createTypedQuery(String string, Class<T> cls,
			boolean sparql) {
		return new TypedQueryImpl<T>(string, cls, new OWLAPIv3OWL2Ontology(m,
				reasoningOnt, r), sparql, this);
	}

	class ICEvaluator {
		public boolean isSatisfied(IntegrityConstraint check) {

			OWL2Ontology<OWLObject> ont = new OWLAPIv3OWL2Ontology(m,
					workingOnt, r);
			OWLAPIv3QueryFactory fact = new OWLAPIv3QueryFactory(m, workingOnt);

			ICQueryGenerator v = new ICQueryGenerator(fact, ont);
			check.accept(v);

			return OWL2QueryEngine.exec(v.getQuery()).isEmpty();
		}

	}
}
