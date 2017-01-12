//
//  PrologState.h
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Prolog.h"


@interface PrologState : NSObject
{
    NSMutableDictionary* map;
    word blob;
}
-(id)initWithTerm:(word)term;
+(PrologState*)emptyState;
-(NSDictionary*)getProperties;
-(word)blob;
-(void)freeState;
-(PrologState*)cloneWith:(word)term;
-(word)getValueFor:(NSString*)key;
@end
