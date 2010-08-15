package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class SingularAttributeImpl<X, T> implements SingularAttribute<X, T> {

	private final boolean id;

	private final String name;

	private final Type<T> type;

	private final Field m;

	private final ManagedType<X> dt;

	private final PersistentAttributeType pat;

	private final IRI iri;

	private final CascadeType[] cascadeTypes;

	private final FetchType fetchType;

	private boolean inferred;

	private ParticipationConstraint[] constraints;

	SingularAttributeImpl(ManagedType<X> declaringType, boolean id,
			String name, IRI iri, Type<T> type, Field m,
			final PersistentAttributeType pat,
			final CascadeType[] cascadeTypes, final FetchType fetchType,
			final boolean inferred, final ParticipationConstraint[] constraints) {
		this.id = id;
		this.name = name;
		this.type = type;
		this.pat = pat;
		this.m = m;
		this.dt = declaringType;
		this.iri = iri;
		this.cascadeTypes = cascadeTypes;
		this.fetchType = fetchType;
		this.inferred = inferred;
		this.constraints = constraints;
	}

	@Override
	public Type<T> getType() {
		return type;
	}

	@Override
	public boolean isId() {
		return id;
	}

	@Override
	public boolean isOptional() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isVersion() {
		throw new UnsupportedOperationException();
	}

	@Override
	public ManagedType<X> getDeclaringType() {
		return dt;
	}

	@Override
	public Member getJavaMember() {
		return m;
	}

	@Override
	public Class<T> getJavaType() {
		return type.getJavaType();
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Attribute.PersistentAttributeType getPersistentAttributeType() {
		return pat;
	}

	@Override
	public boolean isAssociation() {
		return getPersistentAttributeType().equals(
				PersistentAttributeType.OBJECT);
	}

	@Override
	public boolean isCollection() {
		return false;
	}

	@Override
	public Class<T> getBindableJavaType() {
		return type.getJavaType();
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Bindable.BindableType getBindableType() {
		return BindableType.SINGULAR_ATTRIBUTE;
	}

	@Override
	public Field getJavaField() {
		return m;
	}

	@Override
	public IRI getIRI() {
		return iri;
	}

	@Override
	public CascadeType[] getCascadeTypes() {
		return cascadeTypes;
	}

	@Override
	public FetchType getFetchType() {
		return fetchType;
	}

	@Override
	public String toString() {
		return "SingularAttribute[" + name + "]";
	}

	public void setInferred(boolean inferred) {
		this.inferred = inferred;
	}

	public boolean isInferred() {
		return inferred;
	}

	public ParticipationConstraint[] getConstraints() {
		return constraints;
	}
}
