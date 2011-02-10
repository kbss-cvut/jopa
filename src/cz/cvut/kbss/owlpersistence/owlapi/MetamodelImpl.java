package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.owlpersistence.model.annotations.Sequence;
import cz.cvut.kbss.owlpersistence.model.annotations.Types;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintFactory;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.EmbeddableType;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute.PersistentAttributeType;

public class MetamodelImpl implements Metamodel {

	private static final Logger LOG = Logger.getLogger(Metamodel.class
			.getName());

	private final Map<Class<?>, EntityType<?>> typeMap = new HashMap<Class<?>, EntityType<?>>();

	@SuppressWarnings(value = "unused")
	private EntityManagerFactoryImpl emf;

	private static final Set<Class<?>> entities = new HashSet<Class<?>>();

	MetamodelImpl(final EntityManagerFactoryImpl emf) {
		this.emf = emf;
		build();
	}

	private void build() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Building metamodel ... ");
		}

		for (final Class<?> entity : entities) {
			processOWLClass(entity);
		}
	}

	private static Collection<Class<?>> validIdClasses = Arrays
			.asList(new Class<?>[] { String.class, URI.class });

	<X> void processOWLClass(final Class<X> cls) {
		if (typeMap.containsKey(cls)) {
			return;
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("processing OWL class : " + cls);
		}

		final OWLClass c = cls.getAnnotation(OWLClass.class);

		if (c == null) {
			throw new OWLPersistenceException("The class " + cls
					+ " is not an OWLPersistence entity !");
		}

		final EntityTypeImpl<X> c2 = new EntityTypeImpl<X>(cls.getSimpleName(),
				cls, IRI.create(c.iri()));

		typeMap.put(cls, c2);

		for (final Field field : cls.getDeclaredFields()) {
			if (LOG.isLoggable(Level.FINE)) {
				LOG.fine("   processing field : " + field);
			}

			if (field.getType().isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			final Class<?> cxx;

			if (Collection.class.isAssignableFrom(field.getType())) {
				cxx = getSetOrListErasureType((ParameterizedType) field
						.getGenericType());
			} else {
				cxx = field.getType();
			}
			field.setAccessible(true);

			Types tt = field.getAnnotation(Types.class);
			if (tt != null) {
				if (!Set.class.isAssignableFrom(field.getType())) {
					throw new OWLPersistenceException(
							"The Types element must be a set of Strings.");
				}
				c2.addDirectTypes(new TypesSpecificationImpl(c2,
						tt.fetchType(), field, cxx, tt.inferred()));
				continue;
			}

			cz.cvut.kbss.owlpersistence.model.annotations.Properties properties = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.model.annotations.Properties.class);
			if (properties != null) {
				if (!Map.class.isAssignableFrom(field.getType())) {
					throw new OWLPersistenceException(
							"The Types element must be a Map<String,Set<String>>.");
				}
				c2.addOtherProperties(new PropertiesSpecificationImpl(c2,
						properties.fetchType(), field, cxx, true)); // only
																	// inferred
				// @Properties annotation is supported to preserve Domain/Range
				// constraints
				continue;
			}

			OWLObjectProperty oop = field
					.getAnnotation(OWLObjectProperty.class);
			OWLDataProperty odp = field.getAnnotation(OWLDataProperty.class);
			OWLAnnotationProperty oap = field
					.getAnnotation(OWLAnnotationProperty.class);

			cz.cvut.kbss.owlpersistence.model.metamodel.Type<?> type = null;
			PersistentAttributeType t = null;
			IRI iri = null;
			CascadeType[] cascadeTypes = new CascadeType[] {};
			FetchType fetchType = FetchType.LAZY;
			boolean inferred = false;

			ParticipationConstraint[] ics = null;
			ParticipationConstraints cons = field
					.getAnnotation(ParticipationConstraints.class);
			if (cons != null) {
				if ( cons.value() != null ) {
					ics = cons.value();
				}				
			}

			if ( ics ==null) {
			ics = new ParticipationConstraint[]{};
			}


			if (oop != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("     Object property: " + oop);
				}
				t = PersistentAttributeType.OBJECT;
				iri = IRI.create(oop.iri());
				cascadeTypes = oop.cascade();
				processOWLClass(cxx);
				type = typeMap.get(cxx);
				inferred = oop.inferred();
			} else if (odp != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("     Data property: " + odp);
				}
				t = PersistentAttributeType.DATA;
				iri = IRI.create(odp.iri());
				type = BasicTypeImpl.get(cxx);
				inferred = odp.inferred();
			} else if (oap != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("     Annotation property: " + oap);
				}
				t = PersistentAttributeType.ANNOTATION;
				iri = IRI.create(oap.iri());
				type = BasicTypeImpl.get(cxx);
				inferred = oap.inferred();
			}

			final Attribute<X, ?> a;

			if (t != null) {
				if (field.getType().isAssignableFrom(List.class)) {
					final Sequence os = field.getAnnotation(Sequence.class);

					if (os == null) {
						throw new OWLPersistenceException(
								"Expected OWLSequence annotation for " + field.getName());
					}

					a = new ListAttributeImpl(c2, field.getName(), iri,
							List.class, type, field, t, cascadeTypes, IRI
									.create(os.ClassOWLListIRI()), IRI
									.create(os.ObjectPropertyHasNextIRI()), IRI
									.create(os.ObjectPropertyHasContentsIRI()),
							os.type(), fetchType, inferred,ics);
				} else if (field.getType().isAssignableFrom(Set.class)) {
					processOWLClass(cxx);
					a = new SetAttributeImpl(c2, field.getName(), iri,
							Set.class, type, field, t, cascadeTypes, fetchType,
							inferred,ics);
				} else if (field.getType().isAssignableFrom(Map.class)) {
					throw new IllegalArgumentException("NOT YET SUPPORTED");
				} else {
					a = new SingularAttributeImpl(c2, false, field.getName(),
							iri, type, field, t, cascadeTypes, fetchType,
							inferred,ics);
				}
				c2.addDeclaredAttribute(field.getName(), a);

				continue;
			} else {
				final Id id = field.getAnnotation(Id.class);

				if (id == null) {
					continue;
				}

				if (!validIdClasses.contains(field.getType())) {
					throw new IllegalArgumentException("NOT YET SUPPORTED");
				}
				c2.setIdentifier(new IRIIdentifierImpl(field, id.generated()));
			}
		}

		if (c2.getIdentifier() == null) {
			throw new IllegalArgumentException();
		}

		// try {
		// if (cls.isEnum()) {
		// for (final Object e : cls.getEnumConstants()) {
		// final Field idField = getId(e.getClass());
		// IRI id = IRI.create((URI) idField.get(e));
		//
		// if (id == null) {
		// throw new OWLPersistenceException(
		// "An enum individual must have an id specified");
		// }
		//
		// final OWLNamedIndividual i = m.getOWLDataFactory()
		// .getOWLNamedIndividual(id);
		//
		// enumEntities.put(i, e);
		// instanceCache.put(e, i);
		// }
		// }
		// } catch (IllegalArgumentException e1) {
		// throw new OWLPersistenceException(e1);
		// } catch (IllegalAccessException e1) {
		// throw new OWLPersistenceException(e1);
		// }
	}

	private Class<?> getSetOrListErasureType(final ParameterizedType cls) {
		final Type[] t = cls.getActualTypeArguments();

		if (t.length != 1) {
			throw new OWLPersistenceException(
					"Only valid OWLClass annotated classes can be used as parameters for lists and sets.");
		}

		Type type = t[0];

		if (!(type instanceof Class<?>)) {
			throw new OWLPersistenceException(
					"Only Classes might be valid parameters for generic lists and sets");
		}

		return (Class<?>) type;
	}

	public static void addClass(final Class<?> c) {
		entities.add(c);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <X> EntityType<X> entity(Class<X> cls) {
		if (!typeMap.containsKey(cls)) {
			// TODO
			processOWLClass(cls);
		}

		return (EntityType<X>) typeMap.get(cls);
	}

	@Override
	public <X> EmbeddableType<X> embeddable(Class<X> cls) {
		throw new IllegalArgumentException("Embeddable entities not supported.");
	}

	@Override
	public Set<EmbeddableType<?>> getEmbeddables() {
		return Collections.emptySet();
	}

	@Override
	public Set<EntityType<?>> getEntities() {
		return new HashSet<EntityType<?>>(typeMap.values());
	}

	@Override
	public Set<ManagedType<?>> getManagedTypes() {
		return new HashSet<ManagedType<?>>(typeMap.values());
	}

	@Override
	public <X> ManagedType<X> managedType(Class<X> cls) {
		return entity(cls);
	}
}
