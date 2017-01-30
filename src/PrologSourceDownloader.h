//
//  PrologSourceDownloader.h
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface PrologSourceDownloader : NSObject<NSURLConnectionDelegate>
{
    NSMutableData* data;
    void (^callback)(int);
}
-(id)initWithCallback:(void(^)(int))callback;
@end
