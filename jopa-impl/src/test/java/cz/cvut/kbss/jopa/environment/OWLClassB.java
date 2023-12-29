/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.environment;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;

@OWLClass(iri = Vocabulary.c_OwlClassB)
public class OWLClassB {

	private static final String STR_ATT_FIELD = "stringAttribute";
	private static final String PROPERTIES_FIELD = "properties";

	@Id
	private URI uri;

	@OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute")
	private String stringAttribute;

	@Properties(fetchType = FetchType.LAZY)
	private Map<String, Set<String>> properties;

	public OWLClassB() {
    }

    public OWLClassB(URI uri) {
        this.uri = uri;
    }

	/**
	 * @param uri
	 *            the uri to set
	 */
	public void setUri(URI uri) {
		this.uri = uri;
	}

	/**
	 * @return the uri
	 */
	public URI getUri() {
		return uri;
	}

	public void setStringAttribute(String stringAttribute) {
		this.stringAttribute = stringAttribute;
	}

	public String getStringAttribute() {
		return stringAttribute;
	}

	public void setProperties(Map<String, Set<String>> properties) {
		this.properties = properties;
	}

	public Map<String, Set<String>> getProperties() {
		return properties;
	}

	public static String getClassIri() {
		return OWLClassB.class.getAnnotation(OWLClass.class).iri();
	}

	public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
		return OWLClassB.class.getDeclaredField(STR_ATT_FIELD);
	}

	public static Field getPropertiesField() throws NoSuchFieldException, SecurityException {
		return OWLClassB.class.getDeclaredField(PROPERTIES_FIELD);
	}

	@Override
	public String toString() {
		String out = "OWLClassB: uri = " + uri;
		out += ", stringAttribute = " + stringAttribute;
		return out;
	}
}
