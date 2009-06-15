package cz.cvut.kbss.owlpersistence.owlapi.instrumentation;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.instrument.Instrumentation;
import java.security.ProtectionDomain;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class LazyLoadingTransformer implements ClassFileTransformer {

	public static void premain(String agentArgument,
			Instrumentation instrumentation) {
//		if (agentArgument != null) {
//			String[] args = agentArgument.split(",");
//			Set<E> argSet = new HashSet<E>(Arrays.asList(args));
//
//			if (argSet.contains("time")) {
//				System.out.println("Start at " + new Date());
//				Runtime.getRuntime().addShutdownHook(new Thread() {
//					public void run() {
//						System.out.println("Stop at " + new Date());
//					}
//				});
//			}
//			// ... more agent option handling here
//		}
		
		// registers the lazy loading transformer
		instrumentation.addTransformer(new LazyLoadingTransformer());
	}

	public byte[] transform(ClassLoader loader, String className, Class clazz,
			java.security.ProtectionDomain domain, byte[] bytes) {
//
//		for (int i = 0; i < ignore.length; i++) {
//			if (className.startsWith(ignore[i])) {
//				return bytes;
//			}
//		}
//		return doClass(className, clazz, bytes);
		return null;
	}
//	
//	 private byte[] doClass(String name, Class clazz, byte[] b) {
//	 ClassPool pool = ClassPool.getDefault();
//	 CtClass cl = null;
//	 try {
//	 cl = pool.makeClass(new java.io.ByteArrayInputStream(b));
//	 if (cl.isInterface() == false) {
//	
//	 CtField field = CtField.make(def, cl);
//	 String getLogger = "java.util.logging.Logger.getLogger("
//	 + name.replace('/', '.') + ".class.getName());";
//	 cl.addField(field, getLogger);
//	
//	 CtBehavior[] methods = cl.getDeclaredBehaviors();
//	 for (int i = 0; i < methods.length; i++) {
//	 if (methods[i].isEmpty() == false) {
//	 doMethod(methods[i]);
//	 }
//	 }
//	 b = cl.toBytecode();
//	 }
//	 } catch (Exception e) {
//	 System.err.println("Could not instrument  " + name
//	 + ",  exception : " + e.getMessage());
//	 } finally {
//	 if (cl != null) {
//	 cl.detach();
//	 }
//	 }
//	 return b;
	 }
	// private void doMethod(CtBehavior method)
	// throws NotFoundException, CannotCompileException {
	//	
	// String signature = JavassistHelper.getSignature(method);
	// String returnValue = JavassistHelper.returnValue(method);
	//	
	// method.insertBefore(ifLog + "_log.info(\">> " + signature
	// + ");");
	//	
	// method.insertAfter(ifLog + "_log.info(\"<< " + signature
	// + returnValue + ");");
	// }
	// }
	//	
	//	
	//		
	// // @Override
	// // public byte[] transform(ClassLoader arg0, String arg1, Class<?> arg2,
	// // ProtectionDomain arg3, byte[] arg4)
	// // throws IllegalClassFormatException {
	// // // TODO Auto-generated method stub
	// // return null;
	// // }
	// //
}
