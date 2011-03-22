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

package cz.cvut.kbss.owlpersistence.owlapi.instrumentation;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.util.HashSet;
import java.util.Set;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtBehavior;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.NotFoundException;
import cz.cvut.kbss.owlpersistence.FetchType;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLSequence;

public class LazyLoadingTransformer implements ClassFileTransformer {

	public static void premain(String agentArgument,
			Instrumentation instrumentation) {
		System.out.println("Starting OWLPersistence Agent.");
		instrumentation.addTransformer(new LazyLoadingTransformer());
	}

	String[] ignore = new String[] { "sun/", "java/", "javax/" };

	public byte[] transform(ClassLoader loader, String className,
			Class<?> clazz, java.security.ProtectionDomain domain, byte[] bytes) {
		for (int i = 0; i < ignore.length; i++) {
			if (className.startsWith(ignore[i])) {
				return bytes;
			}
		}

		return doClass(className, clazz, bytes);
	}

	private byte[] doClass(String name, Class<?> clazz, byte[] b) {
		ClassPool pool = ClassPool.getDefault();
		CtClass cl = null;
		try {
			cl = pool.makeClass(new java.io.ByteArrayInputStream(b));
			if (!cl.isInterface()) {

				boolean owlClass = false;

				for (final Object o : cl.getAnnotations()) {
					if (o instanceof OWLClass) {
						owlClass = true;
						continue;
					}
				}

				if (owlClass) {
					Set<CtMethod> setters = new HashSet<CtMethod>();
					Set<CtMethod> getters = new HashSet<CtMethod>();

					System.out.println("Processing OWLClass : " + name);

					for (final CtField f : cl.getDeclaredFields()) {
						final String setterName = "set"
								+ f.getName().substring(0, 1).toUpperCase()
								+ f.getName().substring(1);

						try {
							final CtMethod setter = cl.getMethod(setterName,
									"(" + f.getSignature() + ")V");
							setters.add(setter);
							System.out.println("Registering setter - "
									+ setter.getName());
						} catch (NotFoundException e) {
							;
						}

						final String getterName = "get"
								+ f.getName().substring(0, 1).toUpperCase()
								+ f.getName().substring(1);
						try {
							final CtMethod getter = cl.getMethod(getterName,
									"()" + f.getSignature() + "");
							getters.add(getter);
							System.out.println("Registering getter - "
									+ getter.getName());
						} catch (NotFoundException e) {
							;
						}
					}

					for (CtBehavior method : cl.getDeclaredBehaviors()) {
						if (setters.contains(method)) {
							doSetter(method);
						} else if (getters.contains(method)) {
							doGetter(method);
						}
					}
				}
				b = cl.toBytecode();
			}
		} catch (Exception e) {
			System.err.println("Could not instrument  " + name
					+ ",  exception : " + e.getMessage());
		} finally {
			if (cl != null) {
				cl.detach();
			}
		}
		return b;
	}

	private void doSetter(CtBehavior method) throws NotFoundException,
			CannotCompileException {
		method.insertBefore("System.out.println(\"Setting\");");
	}

	private void doGetter(CtBehavior getter) throws NotFoundException,
			CannotCompileException {
//		gmethod.insertBefore("this."
//				+ method.getName().substring(3).toLowerCase()
//				+ "=\"URI.create(\"http://x\");\"");
//		if (FetchType.LAZY.equals(seq.fetchType())) {
			System.out.println("Registering lazy initialization fetcher for "+ getter.getName().substring(3).toLowerCase());
//			getter.insertBefore(seq.fetchType()."System.out.println(\"Getting\");");
//			
//		} 		
	}
}
