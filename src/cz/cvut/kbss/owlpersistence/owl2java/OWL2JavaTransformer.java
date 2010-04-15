package cz.cvut.kbss.owlpersistence.owl2java;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.HermiT.Reasoner.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLStringLiteral;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import com.sun.codemodel.JAnnotationArrayMember;
import com.sun.codemodel.JClass;
import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JDocComment;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JFieldRef;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JType;
import com.sun.codemodel.JVar;

import cz.cvut.kbss.owlpersistence.model.annotations.Constraints;
import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.annotations.Types;
import cz.cvut.kbss.owlpersistence.model.ic.DataParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintVisitor;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectParticipationConstraint;
import cz.cvut.kbss.owlpersistence.owlapi.DatatypeTransformer;
import cz.cvut.kbss.owlpersistence.util.MappingFileParser;

class ContextDefinition {
	final Set<OWLClass> classes = new HashSet<OWLClass>();
	final Set<IntegrityConstraint> ics = new HashSet<IntegrityConstraint>();
}

public class OWL2JavaTransformer {

	private static final Logger LOG = Logger
			.getLogger(OWL2JavaTransformer.class.getName());

	private static final IRI pIsIntegrityConstraintFor = IRI
			.create("http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor");

	private OWLReasoner r;

	private OWLDataFactory f;

	private OWLOntology merged;

	private Map<OWLClass, JDefinedClass> classes = new HashMap<OWLClass, JDefinedClass>();

	private Map<OWLEntity, JFieldRef> entities = new HashMap<OWLEntity, JFieldRef>();

	private Map<JClass, JAnnotationArrayMember> constraints = new HashMap<JClass, JAnnotationArrayMember>();

	private JDefinedClass voc;

	private Map<String, ContextDefinition> contexts = new HashMap<String, ContextDefinition>();

	// private Map<OWLEntity, JFieldRef> vocabularyMap = new HashMap<OWLEntity,
	// JFieldRef>();

	public Collection<String> listContexts() {
		return contexts.keySet();
	}

	private void setupOWLReasoner() {
		// this.r = new OWLAPIIdentityReasonerFactory().createReasoner(merged);
		// this.r = new StructuralReasonerFactory().createReasoner(merged);
		final OWLReasonerConfiguration c = new SimpleConfiguration();
		this.r = new ReasonerFactory().createReasoner(merged, c);
		// this.r = new
		// PelletReasonerFactory().createNonBufferingReasoner(merged,c);
		this.r.prepareReasoner();
	}

	public OWL2JavaTransformer(final String owlOntologyName,
			final String mappingFile) {
		try {
			// reader
			final OWLOntologyManager m = OWLManager.createOWLOntologyManager();

			if (mappingFile != null) {
				LOG.info("Using mapping file '" + mappingFile + "'.");

				final Map<URI, URI> map = MappingFileParser
						.getMappings(new File(mappingFile));
				m.addIRIMapper(new OWLOntologyIRIMapper() {

					@Override
					public org.semanticweb.owlapi.model.IRI getDocumentIRI(
							org.semanticweb.owlapi.model.IRI ontologyIRI) {
						final URI value = map.get(ontologyIRI.toURI());

						if (value == null) {
							return null;
						} else {
							return org.semanticweb.owlapi.model.IRI
									.create(value);
						}
					}
				});
				LOG.info("Mapping file succesfully parsed.");
			}

			LOG.info("Loading ontology " + owlOntologyName + " ... ");
			m.setSilentMissingImportsHandling(true);
			m.loadOntology(org.semanticweb.owlapi.model.IRI
					.create(owlOntologyName));
			merged = new OWLOntologyMerger(m)
					.createMergedOntology(m, org.semanticweb.owlapi.model.IRI
							.create("http://generated"));

			setupOWLReasoner();

			f = m.getOWLDataFactory();

			LOG.info("Parsing integrity constraints");
			final IntegrityConstraintParser icp = new IntegrityConstraintParserImpl();

			for (final OWLAxiom a : merged.getAxioms()) {
				for (final OWLAnnotation p : a.getAnnotations()) {
					if (!p.getProperty().getIRI().equals(
							pIsIntegrityConstraintFor)) {
						continue;
					}
					final OWLAnnotationValue icContext = p.getValue();

					if (!(icContext instanceof OWLStringLiteral)) {
						LOG
								.warning("Ignoring IC "
										+ a
										+ " for '"
										+ icContext
										+ "'. The context is not a valid string literal.");
						continue;
					}

					final String icContextName = ((OWLStringLiteral) icContext)
							.getLiteral();

					ContextDefinition ctx = contexts.get(icContextName);

					if (ctx == null) {
						ctx = new ContextDefinition();
						contexts.put(icContextName, ctx);
					}

					LOG.config("Found IC " + a + " for context "
							+ icContextName);
					ctx.ics.addAll(icp.parse(a, r, merged));
					ctx.classes.addAll(a.getClassesInSignature());
				}
			}
			LOG.info("Integrity constraints succesfully parsed.");
		} catch (OWLOntologyChangeException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (OWLOntologyCreationException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (UnsupportedICException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private JFieldVar addField(final String name, final JDefinedClass cls,
			final JType fieldType) {
		String newName = name;

		int i = 0;
		while (cls.fields().containsKey(newName)) {
			newName = name + "" + (++i);
		}

		final JFieldVar fvId = cls.field(JMod.PROTECTED, fieldType, newName);
		final String fieldName = fvId.name().substring(0, 1).toUpperCase()
				+ fvId.name().substring(1);
		final JMethod mSetId = cls.method(JMod.PUBLIC, void.class, "set"
				+ fieldName);
		final JVar v = mSetId.param(fieldType, fvId.name());
		mSetId.body().assign(JExpr._this().ref(fvId), v);
		final JMethod mGetId = cls.method(JMod.PUBLIC, fieldType, "get"
				+ fieldName);
		mGetId.body()._return(fvId);
		return fvId;
	}

	private JDefinedClass ensureCreated(final String pkg, final JCodeModel cm,
			final OWLClass c) {
		if (classes.containsKey(c)) {
			return classes.get(c);
		}

		JDefinedClass cls;

		String name = pkg + validJavaID(c.getIRI().getFragment());

		try {
			cls = cm._class(name);

			cls
					.annotate(
							cz.cvut.kbss.owlpersistence.model.annotations.OWLClass.class)
					.param("iri", c.getIRI().toString());

			final JDocComment dc = cls.javadoc();
			dc.add("This class was generated by the OWL2Java tool.");

			// RDFS label
			final JClass fieldType1 = cm.ref(String.class);
			final JFieldVar fvId1 = addField("label", cls, fieldType1);
			fvId1.annotate(OWLAnnotationProperty.class).param("iri",
					OWLRDFVocabulary.RDFS_LABEL.getIRI().toString());

			// @Types Set<String> types;
			final JClass fieldType2 = cm.ref(Set.class).narrow(String.class);
			final JFieldVar fvId2 = addField("types", cls, fieldType2);
			fvId2.annotate(Types.class);

			// @Id public final String id;
			final JClass fieldType = cm.ref(String.class);
			final JFieldVar fvId = addField("id", cls, fieldType);
			fvId.annotate(Id.class);

			// // public final Map<Object,Set<Object>> other;
			// final JClass cSetN = cm.ref(Set.class).narrow(Object.class);
			// final JClass cMapN =
			// cm.ref(Map.class).narrow(cm.ref(Object.class),
			// cSetN);
			// final JFieldVar fv = cls.field(JMod.PRIVATE, cMapN, "others");
			//
			// // getOther()
			// final JMethod m = cls.method(JMod.PUBLIC, cSetN, "findOther");
			// final JVar p = m.param(Object.class, "property");
			// m.body()._return(fv.invoke("get").arg(p));

			for (OWLClass cx : classes.keySet()) {
				JDefinedClass cxc = classes.get(cx);
				if (r.isEntailed(OWLManager.getOWLDataFactory()
						.getOWLSubClassOfAxiom(cx, c))) {
					cxc = cxc._extends(cls);
				} else if (r.isEntailed(OWLManager.getOWLDataFactory()
						.getOWLSubClassOfAxiom(c, cx))) {
					cls = cls._extends(cxc);
				}
			}
		} catch (JClassAlreadyExistsException e) {
			cls = cm._getClass(name);
		}
		classes.put(c, cls);

		return cls;
	}

	private void generateVocabulary(final JCodeModel cm) {
		LOG.info("Generating Vocabulary");

		for (final OWLEntity c : merged.getSignature()) {
			String prefix = "";

			if (c.isOWLClass() || c.isOWLDatatype()) {
				prefix = "c_";
			} else if (c.isOWLDataProperty() || c.isOWLObjectProperty()
					|| c.isOWLAnnotationProperty()) {
				prefix = "p_";
			} else if (c.isOWLNamedIndividual()) {
				prefix = "i_";
			}

			// TODO
			if (c.isOWLAnnotationProperty()) {
				continue;
			}

			String id = prefix + validJavaID(c.getIRI().getFragment());

			while (voc.fields().keySet().contains("s_" + id)) {
				id += "_A";
			}

			final String sFieldName = "s_" + id;

			final JFieldVar fv1 = voc.field(JMod.PUBLIC | JMod.STATIC
					| JMod.FINAL, String.class, sFieldName, JExpr.lit(c
					.getIRI().toString()));
			voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, IRI.class, id, cm
					.ref(IRI.class).staticInvoke("create").arg(fv1));

			entities.put(c, fv1.ref(fv1));
		}
	}

	private static String validJavaID(final String s) {
		return s.trim().replace("-", "_");
	}

	class MaxICRestrictor implements IntegrityConstraintVisitor {

		final OWLClass s;
		final OWLProperty<?, ?> p;
		final OWLObject o;
		int max;
		boolean valid = false;
		String pkg;

		MaxICRestrictor(final OWLClass s, final OWLProperty<?, ?> po,
				final OWLObject oc, int max) {
			this.s = s;
			this.p = po;
			this.o = oc;
			this.max = max;
		}

		@Override
		public void visit(ObjectParticipationConstraint cpc) {
			// if (!r.isEntailed(f.getOWLSubClassOfAxiom(s, cpc.getSubject()))
			// || !r.isEntailed(f.getOWLSubObjectPropertyOfAxiom(po, cpc
			// .getPredicate()))
			// || !r.isEntailed(f.getOWLSubClassOfAxiom(oc, cpc
			// .getObject()))) {
			// return;
			// }
			if (!s.equals(cpc.getSubject()) || !p.equals(cpc.getPredicate())
					|| !o.equals(cpc.getObject())) {
				return;
			}

			valid = true;

			if (cpc.getMax() >= 0) {
				max = Math.min(cpc.getMax(), max);
			}
		}

		@Override
		public void visit(DataParticipationConstraint cpc) {
			// if (!r.isEntailed(f.getOWLSubClassOfAxiom(s, cpc.getSubject()))
			// || !r.isEntailed(f.getOWLSubDataPropertyOfAxiom(pd, cpc
			// .getPredicate())) || !od.equals(cpc.getObject())) {
			// return;
			// }
			if (!s.equals(cpc.getSubject()) || !p.equals(cpc.getPredicate())
					|| !o.equals(cpc.getObject())) {
				return;
			}

			valid = true;

			if (cpc.getMax() >= 0) {
				max = Math.min(cpc.getMax(), max);
			}
		}
	}

	private void generateAttribute(final String pkg, final OWLClass s,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			final OWLClass o, final Collection<IntegrityConstraint> ics,
			final JCodeModel cm) {
		final JDefinedClass subj = ensureCreated(pkg, cm, s);
		final JDefinedClass obj = ensureCreated(pkg, cm, o);

		Set<ObjectParticipationConstraint> annotations = new HashSet<ObjectParticipationConstraint>();

		int max = Integer.MAX_VALUE;

		for (final IntegrityConstraint c : ics) {
			final MaxICRestrictor r = new MaxICRestrictor(s, p, o, max);
			c.accept(r);
			max = Math.min(max, r.max);
			if (r.valid)
				annotations.add((ObjectParticipationConstraint) c);
		}

		final String fieldName = validJavaID(p.getIRI().getFragment());

		if (r.isEntailed(f.getOWLSubClassOfAxiom(s, f
				.getOWLObjectMaxCardinality(1, p)))) {
			max = Math.min(max, 1);
		}

		final JFieldVar fv;

		if (max > 1) {
			fv = addField(fieldName, subj, cm.ref(java.util.Set.class).narrow(
					obj));
		} else {
			fv = addField(fieldName, subj, obj);
		}

		fv.annotate(OWLObjectProperty.class)
				.param("iri", p.getIRI().toString()).param("fillerIri",
						o.getIRI().toString());

		if (!annotations.isEmpty()) {
			JAnnotationArrayMember use = constraints.get(subj);

			if (use == null) {
				use = subj.annotate(Constraints.class).paramArray("value");
				constraints.put(subj, use);
			}

			for (ObjectParticipationConstraint ic : annotations) {
				use.annotate(ParticipationConstraint.class).param(
						"owlClassIRI", ic.getSubject().getIRI().toString())
						.param("owlPropertyIRI",
								ic.getPredicate().getIRI().toString()).param(
								"owlObjectIRI",
								ic.getObject().getIRI().toString()).param(
								"min", ic.getMin()).param("max", ic.getMax());
			}
		}
	}

	private void generateAttribute(final String pkg, final OWLClass s,
			final org.semanticweb.owlapi.model.OWLDataProperty p,
			final OWLDatatype o, final Collection<IntegrityConstraint> ics,
			final JCodeModel cm) {
		final JDefinedClass subj = ensureCreated(pkg, cm, s);
		final JType obj = cm._ref(DatatypeTransformer.transformOWLType(o
				.asOWLDatatype()));

		Set<DataParticipationConstraint> annotations = new HashSet<DataParticipationConstraint>();

		int max = Integer.MAX_VALUE;

		for (final IntegrityConstraint c : ics) {
			final MaxICRestrictor r = new MaxICRestrictor(s, p, o, max);
			c.accept(r);
			max = Math.min(max, r.max);
			if (r.valid)
				annotations.add((DataParticipationConstraint) c);
		}

		final String fieldName = validJavaID(p.getIRI().getFragment());

		if (r.isEntailed(f.getOWLSubClassOfAxiom(s, f.getOWLDataMaxCardinality(
				1, p)))) {
			max = Math.min(max, 1);
		}

		JFieldVar fv;

		if (max > 1) {
			fv = addField(fieldName, subj, cm.ref(java.util.Set.class).narrow(
					obj));
		} else {
			fv = addField(fieldName, subj, obj);
		}

		fv.annotate(OWLDataProperty.class).param("iri", p.getIRI().toString())
				.param("fillerIri", o.getIRI().toString());

		if (!annotations.isEmpty()) {
			JAnnotationArrayMember use = constraints.get(subj);

			if (use == null) {
				use = subj.annotate(Constraints.class).paramArray("value");
				constraints.put(subj, use);
			}

			for (DataParticipationConstraint ic : annotations) {
				use.annotate(ParticipationConstraint.class).param(
						"owlClassIRI", ic.getSubject().getIRI().toString())
						.param("owlPropertyIRI",
								ic.getPredicate().getIRI().toString()).param(
								"owlObjectIRI",
								ic.getObject().getIRI().toString()).param(
								"min", ic.getMin()).param("max", ic.getMax());
			}
		}

	}

	private void generateModel(final JCodeModel cm,
			final ContextDefinition context, final String pkg) {
		LOG.info("Generating model ...");

		// model classes
		try {
			for (org.semanticweb.owlapi.model.OWLObjectProperty op : merged
					.getObjectPropertiesInSignature()) {
				LOG.config("Checking object property '" + op + "'.");

				final Collection<OWLClass> setC = new HashSet<OWLClass>();

				for (OWLClass c : r.getSubClasses(
						OWLManager.getOWLDataFactory()
								.getOWLObjectSomeValuesFrom(
										op,
										OWLManager.getOWLDataFactory()
												.getOWLThing()), true)
						.getFlattened()) {
					if (c.isOWLThing() || c.isOWLNothing()) {
						continue;
					}
					setC.add(c);
				}

				for (OWLClass c : r.getObjectPropertyDomains(op, false)
						.getFlattened()) {
					if (c.isOWLThing() || c.isOWLNothing()) {
						continue;
					}
					setC.add(c);
				}

				setC.retainAll(context.classes);

				System.out.println(" Checking domain parts: " + setC);

				for (OWLClass c : setC) {
					System.out.println("   Checking domain part " + c);

					final Set<OWLClass> ranges = r.getObjectPropertyRanges(op,
							false).getFlattened();

					System.out.println("Ranges of " + op + " are " + ranges);

					for (OWLClass cc : ranges) {
						if (cc.isOWLThing() || cc.isOWLNothing()) {
							continue;
						}

						System.out.println("       Checking range " + cc);

						final Set<OWLClass> cxxx = new HashSet<OWLClass>();
						cxxx.addAll(r.getSubClasses(cc, false).getFlattened());
						cxxx.addAll(r.getEquivalentClasses(cc).getEntities());
						System.out.println("                before: " + cxxx);
						cxxx.retainAll(ranges);

						System.out.println("                after: " + cxxx);

						if (cxxx.size() > 1
								|| !cxxx.iterator().next().equals(cc)) {
							continue;
						}

						System.out.println("       OK - generating attribute.");
						generateAttribute(pkg, c, op, cc, context.ics, cm);
					}
				}
			}

			for (org.semanticweb.owlapi.model.OWLDataProperty op : merged
					.getDataPropertiesInSignature()) {
				LOG.config("Checking data property '" + op + "'.");

				final Collection<OWLClass> setC = new HashSet<OWLClass>();

				boolean generated = false;
				
				for (OWLClass c : r.getSubClasses(
						OWLManager.getOWLDataFactory()
								.getOWLDataSomeValuesFrom(
										op,
										OWLManager.getOWLDataFactory()
												.getTopDatatype()), true)
						.getFlattened()) {
					if (c.isOWLThing() && generated) {
						continue;
					}
					
					generated = true;
					
					setC.add(c);
				}

				generated = false;
				
				for (OWLClass c : r.getDataPropertyDomains(op, false)
						.getFlattened()) {
					if (c.isOWLThing() && generated) {
						continue;
					}
					
					generated = true;
					
					setC.add(c);
				}

				setC.retainAll(context.classes);

				System.out.println(" Checking domain parts: " + setC);

				for (OWLClass c : setC) {
					System.out.println("   Checking domain " + c);
					
					boolean generated2 = false;
					
					for (OWLDatatype cc : merged.getDatatypesInSignature()) {
						if ( cc.isTopDatatype() && generated2 ) {
							continue;
						}
						
						System.out.println("   Checking range " + cc);
						if (!r.isEntailed(f
								.getOWLDataPropertyRangeAxiom(op, cc))) {
							continue;
						}
						
						generated2 = true;

						System.out.println("       OK - generating attribute.");
						generateAttribute(pkg, c, op, cc, context.ics, cm);
					}
				}
			}
		} catch (UnsupportedICException e) {
			e.printStackTrace();
		}
	}

	public void transform(String context, String p, String dir) {
		LOG.info("Transforming context '" + p + "'.");

		final JCodeModel cm = new JCodeModel();

		try {
			voc = cm._class(p + ".Vocabulary");
			generateVocabulary(cm);
			generateModel(cm, contexts.get(context), p + ".model.");

			final File file = new File(dir);
			file.mkdirs();
			cm.build(file);
			LOG.info("Transformation SUCCESFUL.");
		} catch (JClassAlreadyExistsException e1) {
			LOG.log(Level.SEVERE, "Transformation FAILED.", e1);
		} catch (IOException e) {
			LOG.log(Level.SEVERE, "File generation FAILED.", e);
		}
	}
}
