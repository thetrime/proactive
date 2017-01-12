//
//  Constants.h
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Prolog.h"

@interface Constants : NSObject
+(word)emptyListAtom;
+(word)curlyAtom;
+(word)nullAtom;
+(word)prologStateKeyAtom;
+(word)atomAtom;
+(word)attributeAtom;
+(word)listAtom;

+(word)equalsFunctor;
+(word)renderFunctor;
+(word)getInitialStateFunctor;
+(word)crossModuleFunctor;
+(word)expandChildrenFunctor;
+(word)curlyFunctor;
+(word)commaFunctor;
+(word)colonFunctor;
+(word)errorFunctor;
+(word)typeErrorFunctor;
+(word)listFunctor;
+(word)documentFunctor;
+(word)createElementFromVdomFunctor;

+(word)makeNull;
+(bool)isNull:(word)w;
@end
