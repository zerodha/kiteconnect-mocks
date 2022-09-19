// To parse this JSON:
//
//   NSError *error;
//   GttDeleteOrder * = [GttDeleteOrder fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class GttDeleteOrder;
@class GttDeleteOrderData;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface GttDeleteOrder : NSObject
@property (nonatomic, nullable, strong) GttDeleteOrderData *data;
@property (nonatomic, nullable, copy)   NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface GttDeleteOrderData : NSObject
@property (nonatomic, nullable, strong) NSNumber *triggerID;
@end

NS_ASSUME_NONNULL_END
