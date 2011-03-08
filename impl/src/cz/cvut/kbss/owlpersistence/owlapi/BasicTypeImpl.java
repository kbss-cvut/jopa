package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.metamodel.BasicType;

public class BasicTypeImpl<X> implements BasicType<X> {

	private Class<X> c;

	BasicTypeImpl(Class<X> c) {
		this.c = c;
	}

	@Override
	public Class<X> getJavaType() {
		return c;
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Type.PersistenceType getPersistenceType() {
		return PersistenceType.BASIC;
	}

	public static <X> BasicType<X> get(final Class<X> c) {
		return new BasicTypeImpl<X>(c);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((c == null) ? 0 : c.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		BasicTypeImpl other = (BasicTypeImpl) obj;
		if (c == null) {
			if (other.c != null)
				return false;
		} else if (!c.equals(other.c))
			return false;
		return true;
	}
}
