package cz.cvut.kbss.owlpersistence.owlapi.fresh;

import java.net.URI;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;

public class DatatypeTransformer {

	private static Map<OWL2Datatype, Class<?>> map = new HashMap<OWL2Datatype, Class<?>>();

	static {
		map.put(OWL2Datatype.XSD_STRING, String.class);
		map.put(OWL2Datatype.RDF_XML_LITERAL, String.class);
		map.put(OWL2Datatype.XSD_INT, Integer.class);
		map.put(OWL2Datatype.XSD_INTEGER, Integer.class);
		map.put(OWL2Datatype.XSD_DOUBLE, Double.class);
		map.put(OWL2Datatype.XSD_FLOAT, Float.class);
		map.put(OWL2Datatype.XSD_BOOLEAN, Boolean.class);
		map.put(OWL2Datatype.XSD_DATE_TIME, Date.class);
		map.put(OWL2Datatype.XSD_DATE_TIME_STAMP, Date.class);
		map.put(OWL2Datatype.XSD_SHORT, Short.class);
		map.put(OWL2Datatype.XSD_LONG, Long.class);
		map.put(OWL2Datatype.XSD_ANY_URI, URI.class);
	}

	public static Class<?> transformOWLType(final OWLDatatype dt) {
		Class<?> type = null;

		if (dt.isBuiltIn()) {
			if (dt.getIRI().toString().equals(
					"http://www.w3.org/2000/01/rdf-schema#Literal")) {
				type = Object.class;
			} else {
				type = map.get(dt.getBuiltInDatatype());
			}
		}

		if (type == null) {
			throw new IllegalArgumentException("Unsupported datatype: " + dt);
		}

		return type;
	}

	public static Object transform(final OWLLiteral l) {
		if (l.getDatatype().isBuiltIn())
			switch (l.getDatatype().getBuiltInDatatype()) {
			case XSD_SHORT:
				return Short.parseShort(l.getLiteral());
			case XSD_LONG:
				return Long.parseLong(l.getLiteral());
			case XSD_INT:
			case XSD_INTEGER:
				return Integer.parseInt(l.getLiteral());
			case XSD_DOUBLE:
			case XSD_DECIMAL:
				return Double.parseDouble(l.getLiteral());
			case XSD_FLOAT:
				return Float.parseFloat(l.getLiteral());
			case XSD_STRING:
			case RDF_XML_LITERAL:
				return l.getLiteral();
			case XSD_BOOLEAN:
				return Boolean.parseBoolean(l.getLiteral());
			case XSD_ANY_URI:
				return URI.create(l.getLiteral());
			case XSD_DATE_TIME_STAMP:
			case XSD_DATE_TIME:
				try {
					return new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss")
							.parse(l.getLiteral());
				} catch (ParseException e) {
					throw new OWLPersistenceException("The date time '"
							+ l.getLiteral() + "' cannot be parsed");
				}
			}

		throw new IllegalArgumentException("Unsupported datatype: "
				+ l.getDatatype());
	}

	public static boolean isSupportedJavaType(Class<?> dt) {
		return map.values().contains(dt);
	}
}
